#!/usr/bin/env python
# Copyright 2009 by Ka-Ping Yee <ping@zesty.ca>
#
# multitet is free software: you can redistribute and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation (either version 3, or, at your option, any later version).
#
# multitet is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for details.
# You should have received a copy of the GNU General Public License along
# with multitet.  If not, see <http://www.gnu.org/licenses/>.

# Naming convention: x and y always refer to coordinates in pixels, not in
# grid space.  For grids, c refers to a column number, r refers to a row
# number, and cr refers to a (column, row) pair.

from libavg import avg, Point2D, AVGApp, AVGNode, fadeOut
from libavg.AVGAppUtil import getMediaDir
from buttons import LabelButton
import math, random

def set_handler(node, type, handler):
    node.setEventHandler(type, avg.MOUSE | avg.TOUCH, handler)

def start_capture(node, cursor_id):
    node.setEventCapture(cursor_id)

def end_capture(node, cursor_id):
    node.releaseEventCapture(cursor_id)

def create_node(parent_node, type, **props):
    node = avg.Player.get().createNode(type, props)
    parent_node.appendChild(node)
    return node

def raise_node(node):
    parent_node = node.getParent()
    parent_node.removeChild(node)
    parent_node.appendChild(node)

def lower_node(node):
    parent_node = node.getParent()
    parent_node.removeChild(node)
    parent_node.insertChild(node, 0)

def create_button(parent_node, callback, **props):
    button = LabelButton(
        parent_node, props.get('pos', Point2D(0, 0)),
        props.get('text', 'Button'), props.get('fontsize', 20), callback)
    for key in props:
        setattr(button._node, key, props[key])
    return button

def set_timeout(millis, handler):
    return avg.Player.get().setTimeout(millis, handler)

def set_interval(millis, handler):
    return avg.Player.get().setInterval(millis, handler)

def clear_interval(id):
    avg.Player.get().clearInterval(id)

def get_centroid(points):
    return sum(points, Point2D(0.0, 0.0))/len(points)

def get_heading(origin, point):
    """Get the direction of the screen vector from 'origin' to 'point' as a
    value in degrees from 0 to 360, where 0 is north and 90 is east."""
    vector = point - origin
    radians = math.atan2(-vector.y, vector.x)  # 0 = east, pi/2 = north
    degrees = radians*180/math.pi  # 0 = east, 90 = north
    heading = 90 - degrees  # 0 = north, 90 = east
    return (heading + 360) % 360

def add_cr((c1, r1), (c2, r2)):
    """Add two (column, row) pairs together."""
    return (c1 + c2, r1 + r2)

def transitive_closure(marked_nodes, edges):
    """Find the transitive closure of a set of nodes over a graph.  'edges'
    expresses the graph as a dictionary mapping each node N to the set of nodes
    that have edges from N.  'marked_nodes' is the initially marked subset."""
    closure = set()
    while marked_nodes:
        closure = closure.union(marked_nodes)
        for node in closure:
            marked_nodes = marked_nodes.union(edges[node])
        marked_nodes -= closure
    return closure

class Grid:
    """A fixed-size rectangular array of cells, where a false value represents
    the absence of a block and anything else represents a block.  Clients of
    this class may use characters or object references to represent blocks."""

    def __init__(self, ncolumns, nrows, cells=None):
        self.ncolumns = ncolumns
        self.nrows = nrows
        if cells is None:
            self.clear()
        else:
            self.cells = [list(row) for row in cells]
        assert len(self.cells) == self.nrows
        for row in self.cells:
            assert len(row) == self.ncolumns

    def dump(self, name):
        for row in self.cells:
            line = name + ': '
            for cell in row:
                if isinstance(cell, Piece):
                    ch = 'P'
                elif isinstance(cell, AVGNode):
                    ch = 'N'
                elif cell is None:
                    ch = '-'
                else:
                    ch = str(cell)[0]
                line += ch
            print line

    def clear(self):
        self.cells = [[None]*self.ncolumns for r in range(self.nrows)]

    def get(self, (c, r)):
        """Get the value of the cell in column c of row r."""
        return self.cells[r][c]

    def in_bounds(self, (c, r)):
        """Return True if the given position is in bounds for this Grid."""
        return 0 <= c < self.ncolumns and 0 <= r < self.nrows

    def put(self, (c, r), value):
        """Get the value of the cell in column c of row r."""
        self.cells[r][c] = value

    def put_all(self, grid, (c, r), override_value=None):
        """Write the blocks in the given Grid into this Grid, placing the NW
        corner of the given Grid at (c, r) in this Grid.  If 'override_value'
        is specified, write it instead of the values in the given Grid."""
        for (cc, rr), value in grid.get_blocks():
            self.cells[r + rr][c + cc] = override_value or value

    def remove_value(self, value):
        """Remove all blocks with the given value from this Grid."""
        for r in range(self.nrows):
            for c in range(self.ncolumns):
                if self.cells[r][c] == value:
                    self.cells[r][c] = None

    def overlaps_any(self, grid, (c, r), ignore_value=None):
        """Return True if this Grid overlaps any blocks in the given Grid, when
        placed with its NW corner at (c, r) in this Grid.  If 'ignore_value'
        is specified, blocks in this Grid with this value are ignored."""
        for (cc, rr), value in grid.get_blocks():
            value = self.cells[r + rr][c + cc]
            if value and value != ignore_value:
                return True

    def overlaps_all(self, grid, (c, r)):
        """Return True if this Grid overlaps all blocks in the given Grid, when
        placed with its NW corner at (c, r) in this Grid."""
        for (cc, rr), value in grid.get_blocks():
            if not self.cells[r + rr][c + cc]:
                return False
        return True

    def all_in_bounds(self, grid, (c, r)):
        """Return True if all blocks of the given Grid are in bounds when the
        NW corner of the given Grid is placed at (c, r) in this Grid."""
        for (cc, rr), value in grid.get_blocks():
            if not self.in_bounds((c + cc, r + rr)):
                return False
        return True

    def get_blocks(self):
        """Generate (cr, value) pairs for all the blocks in this grid."""
        for r in range(self.nrows):
            for c in range(self.ncolumns):
                if self.cells[r][c] is not None:
                    yield (c, r), self.cells[r][c]

    def get_rotated(self, cw_quarter_turns):
        """Create a new Grid object that contains a copy of this Grid, rotated
        clockwise by the given number of quarter turns."""
        turns = cw_quarter_turns % 4
        if turns == 0:
            return Grid(self.ncolumns, self.nrows, self.cells)
        if turns == 2:
            rotated_cells = [reversed(row) for row in reversed(self.cells)]
            return Grid(self.ncolumns, self.nrows, rotated_cells)
        if turns == 1:
            rotated_cells = [
                [self.cells[r][c] for r in reversed(range(self.nrows))]
                for c in range(self.ncolumns)]
            return Grid(self.nrows, self.ncolumns, rotated_cells)
        if turns == 3:
            rotated_cells = [
                [self.cells[r][c] for r in range(self.nrows)]
                for c in reversed(range(self.ncolumns))]
            return Grid(self.nrows, self.ncolumns, rotated_cells)

def make_cells(str):
    return [[ch != '-' and ch or None for ch in row] for row in str.split()]

PIECE_GRIDS = [
# Crazy pieces!
#   Grid(5, 5, make_cells('----- -LLL- -L--- -L--- -----')),
#   Grid(5, 5, make_cells('----- --SSS --SS- ----- -----')),
#   Grid(5, 5, make_cells('----- -TTT- --T-- --T-- -----')),
#   Grid(5, 5, make_cells('----- --ZZ- -ZZZ- ----- -----')),
#   Grid(5, 5, make_cells('----- -IIII --I-- ----- -----')),
#   Grid(5, 5, make_cells('----- IIII- --I-- ----- -----')),
#   Grid(5, 5, make_cells('----- -XXX- -XX-- ----- -----')),
#   Grid(5, 5, make_cells('----- -XX-- -XXX- ----- -----')),

    Grid(4, 4, make_cells('---- -FF- -F-- -F--')),  # F-shape
    Grid(4, 4, make_cells('-L-- -L-- -LL- ----')),  # L-shape
    Grid(4, 4, make_cells('--I- --I- --I- --I-')),  # I-shape
    Grid(3, 3, make_cells('-SS SS- ---')),  # S-shape
    Grid(3, 3, make_cells('ZZ- -ZZ ---')),  # Z-shape
    Grid(3, 3, make_cells('--- TTT -T-')),  # T-shape
    Grid(2, 2, make_cells('XX XX')),  # X-shape
]

STARTING_ZONE_NROWS = 1
MANIPULATOR_RADIUS = 1.8  # radius of the circular manipulator, in block units

class Piece:
    """A falling game piece.  This class maintains a Grid for the shape of
    the piece and Nodes that display the piece, and handles player events
    that manipulate the Piece."""

    def __init__(self, parent_node, board, cr, grid):
        """'parent_node' is the parent AVGNode; 'board' is the game board;
        'cr' is the position of the NW corner of this piece on the board;
        'grid' is a Grid of strings that refer to block images."""

        self.parent_node = parent_node
        self.board = board

        self.touches = {}  # Current positions of touches on this piece.

        self.grab_centroid = None  # Centroid of initial grab points.
        self.grab_cr = None  # Initial grid position when grabbed.

        self.grab_heading = None  # Direction indicated by initial grab points.
        self.grab_grid = None  # Initial grid shape when grabbed.

        # Just create the nodes; self.move_to() will position them.
        self.block_nodes = []
        for block in grid.get_blocks():
            block_node = create_node(self.parent_node, 'image')
            set_handler(block_node, avg.CURSORDOWN, self.handle_down)
            set_handler(block_node, avg.CURSORMOTION, self.handle_motion)
            set_handler(block_node, avg.CURSORUP, self.handle_up)
            self.block_nodes.append(block_node)

        # Provide a circular manipulator to make the piece easier to grab.
        # The manipulator is initially behind all other nodes.
        self.manip_node = create_node(self.parent_node, 'circle',
            r=self.board.scale*MANIPULATOR_RADIUS,
            color='000000', opacity=0, fillcolor='ffffff', fillopacity=0)
        set_handler(self.manip_node, avg.CURSORDOWN, self.handle_down)
        set_handler(self.manip_node, avg.CURSORMOTION, self.handle_motion)
        set_handler(self.manip_node, avg.CURSORUP, self.handle_up)

        self.move_to(cr, grid)

    def destroy(self):
        """Remove this Piece from the display."""
        for node in self.block_nodes:
            node.unlink()
        self.manip_node.unlink()

    def get_blocks(self):
        """Get the grid positions of the blocks that make up this piece."""
        for cr, value in self.grid.get_blocks():
            yield add_cr(self.cr, cr), value

    def is_grabbed(self):
        return self.touches

    def update_blocks(self):
        """Update the image nodes for this piece's blocks."""
        for node, (cr, value) in zip(self.block_nodes, self.get_blocks()):
            node.href = 'falling-%s.png' % value
            node.pos = self.board.get_nw_point(cr)
            node.size = self.board.block_size

    def update_manip(self):
        """Update the display of the piece's manipulator."""
        center = self.board.get_nw_point(
            add_cr(self.cr, (self.grid.ncolumns*0.5, self.grid.nrows*0.5)))
        self.manip_node.pos = center

        if self.touches:
            raise_node(self.manip_node)
            self.manip_node.fillopacity = 0.15 * len(self.touches)
        else:
            lower_node(self.manip_node)
            self.manip_node.fillopacity = 0

    def handle_down(self, event):
        if len(self.touches) == 2:  # Ignore third and subsequent touches.
            return
        self.touches[event.cursorid] = event.pos
        start_capture(self.block_nodes[0], event.cursorid)

        # On the first or second touch, start a translation grab.
        self.grab_centroid = get_centroid(self.touches.values())
        self.grab_cr = self.cr

        # On the second touch, also start a rotation grab. 
        if len(self.touches) == 2:
            self.grab_heading = get_heading(*self.touches.values())
            self.grab_grid = self.grid

        self.update_manip()

    def handle_up(self, event):
        if event.cursorid in self.touches:
            del self.touches[event.cursorid]
            end_capture(self.block_nodes[0], event.cursorid)

        if len(self.touches) == 1:  # Reset any remaining translation grab.
            self.grab_centroid = get_centroid(self.touches.values())
            self.grab_cr = self.cr

        self.update_manip()

    def handle_motion(self, event):
        if event.cursorid not in self.touches:  # Ignore extraneous touches.
            return

        self.touches[event.cursorid] = event.pos
        new_grid = self.grid

        # Always update translation.
        new_centroid = get_centroid(self.touches.values())
        delta_centroid = new_centroid - self.grab_centroid
        delta_c = int(round(delta_centroid.x/self.board.scale))
        delta_r = int(round(delta_centroid.y/self.board.scale))
        new_cr = add_cr(self.grab_cr, (delta_c, max(0, delta_r)))

        # If there are two touches, update rotation.
        if len(self.touches) == 2:
            new_heading = get_heading(*self.touches.values())
            delta_heading = new_heading - self.grab_heading
            cw_quarter_turns = int(round(delta_heading/90))
            new_grid = self.grab_grid.get_rotated(cw_quarter_turns)

        if self.board.can_put_piece(self, new_cr, new_grid):
            self.move_to(new_cr, new_grid)

    def move_to(self, cr, grid=None):
        """Move this Piece to the given grid position, optionally with a new
        grid of blocks, updating the display and the Board.  The caller is
        responsible for avoiding overlaps with existing blocks on the Board."""
        self.cr = cr
        self.grid = grid or self.grid
        self.update_blocks()
        self.update_manip()
        self.board.put_piece(self)

class Board:
    """The game board.  This class maintains a Grid for the pieces on the
    board and a Grid for the frozen blocks on the board."""

    def __init__(self, parent_node, ncolumns, nrows, pos, scale):
        self.parent_node = parent_node
        self.ncolumns = ncolumns
        self.nrows = nrows
        self.pos = pos
        self.size = Point2D(ncolumns*scale, nrows*scale)
        self.scale = float(scale)
        self.block_size = Point2D(self.scale, self.scale)
        self.piece_grid = Grid(ncolumns, nrows)  # contains Piece references
        self.frozen_grid = Grid(ncolumns, nrows)  # contains Node references

    def destroy(self):
        for cr, value in self.frozen_grid.get_blocks():
            value.unlink()

    def get_nw_point(self, (c, r)):
        return self.pos + Point2D(c, r)*self.scale

    def get_cr(self, point):
        cr_float = (point - self.pos)/self.scale
        return (int(cr_float.x), int(cr_float.y))

    def can_put_piece(self, piece, cr, grid):
        """Check whether a piece can be placed on the piece grid."""
        return (self.piece_grid.all_in_bounds(grid, cr) and
                not self.piece_grid.overlaps_any(grid, cr, piece) and
                not self.frozen_grid.overlaps_any(grid, cr))

    def put_piece(self, piece):
        """Put a piece on the piece grid, removing it from its old location."""
        self.piece_grid.remove_value(piece)
        self.piece_grid.put_all(piece.grid, piece.cr, piece)

    def freeze_piece(self, piece):
        """Copy a piece to the frozen grid, and destroy the piece."""
        self.piece_grid.remove_value(piece)
        for cr, value in piece.get_blocks():
            self.frozen_grid.put(cr, create_node(self.parent_node, 'image',
                size=self.block_size, href='frozen-%s.png' % value,
                pos=self.get_nw_point(cr)))
        piece.destroy()

    def delete_frozen(self, grid):
        """Delete the specified frozen blocks, and let the frozen blocks above
        them fall down one space, unless they would collide with pieces."""
        # This algorithm relies on get_blocks returning blocks in top-to-bottom
        # order (so deleting one block does not affect future deletions).
        for (c, r), value in grid.get_blocks():
            node = self.frozen_grid.get((c, r))
            if node:
                node.unlink()
                self.frozen_grid.put((c, r), None)
                for rr in reversed(range(r)):
                    if self.piece_grid.get((c, rr + 1)):
                        break
                    self.move_frozen_block((c, rr), (c, rr + 1))

    def move_frozen_block(self, cr, new_cr):
        """Move a single frozen block to a new location."""
        node = self.frozen_grid.get(cr)
        if node:
            self.frozen_grid.put(new_cr, node)
            self.frozen_grid.put(cr, None)
            node.pos = self.get_nw_point(new_cr)

    def highlight_dissolving(self, grid):
        """Highlight frozen blocks to indicate that they are dissolving."""
        for (c, r), value in grid.get_blocks():
            node = self.frozen_grid.get((c, r))
            if node:
                node.href = 'dissolving.png'

class Level:
    """The controller for one round of the game.  This class holds the Board
    and the list of Pieces, and causes the Pieces to fall with each clock tick.
    When the fall of a Piece is stopped by blocks below, the Piece "freezes":
    the blocks of the Piece are copied onto the board and the Piece object is
    destroyed.  Pieces appear in a "starting zone" at the top where they cannot
    be manipulated; if a piece freezes in the starting zone, the round ends."""

    def __init__(self, parent_node, scale, app,
                 section_pattern, tick_interval, ticks_per_piece):
        self.width, self.height = parent_node.size.x, parent_node.size.y
        self.node = create_node(parent_node, 'div')
        self.section_node = create_node(self.node, 'div', sensitive=False)
        self.board = Board(self.node,
                           int(self.width/scale), int(self.height/scale),
                           Point2D(0, self.height % scale), scale)
        self.board_rect = create_node(self.node, 'rect',
            pos=self.board.pos, size=self.board.size,
            color='a0a0a0', strokewidth=4, sensitive=False)

        self.app = app
        self.interval_id = None
        self.tick_interval = tick_interval
        self.ticks_per_piece = ticks_per_piece
        self.ticks_to_next_piece = 1

        self.pieces = []
        self.dissolving = Grid(self.board.ncolumns, self.board.nrows)

        row_cells = 'X'*self.board.ncolumns
        self.starting_zone = Grid(self.board.ncolumns, STARTING_ZONE_NROWS,
            [row_cells]*STARTING_ZONE_NROWS)

        self.section_nodes = []
        self.set_section_pattern(section_pattern)

        # The starting zone can be touched to request more pieces.
        self.starting_zone_node = create_node(parent_node, 'rect',
            size=Point2D(self.board.ncolumns, STARTING_ZONE_NROWS)*scale,
            pos=self.board.pos, opacity=0, fillcolor='ff0000', fillopacity=0.2)
        set_handler(self.starting_zone_node, avg.CURSORDOWN, self.handle_down)
        self.can_add_pieces = True  # For debouncing taps on the starting zone.

    def handle_down(self, event):
        if self.can_add_pieces:
            self.starting_zone_node.fillopacity = 0.4
            self.add_piece(self.board.get_cr(event.pos)[0])
            self.can_add_pieces = False  # Ignore extraneous double-taps.
            set_timeout(300, self.reset_starting_zone)

    def reset_starting_zone(self):
        self.starting_zone_node.fillopacity = 0.2
        self.can_add_pieces = True

    def set_section_pattern(self, section_pattern):
        for node in self.section_nodes:
            node.unlink()

        self.section_nodes = []
        self.sections = []
        s = 0

        for r in reversed(range(STARTING_ZONE_NROWS, self.board.nrows)):
            nsections = section_pattern[s]
            s = (s + 1) % len(section_pattern)

            section_min = 0
            for i in range(nsections):
                section_max = int(self.board.ncolumns*float(i + 1)/nsections)
                section_ncolumns = section_max - section_min

                section = Grid(self.board.ncolumns, self.board.nrows)
                for j in range(section_min, section_max):
                    section.put((j, r), 'X')
                self.sections.append(section)

                self.section_nodes.append(create_node(self.section_node, 'rect',
                    pos=self.board.get_nw_point((section_min, r)),
                    size=Point2D(section_ncolumns, 1)*self.board.scale,
                    opacity=0.2,
                    color='ffffff',
                    sensitive=False))

                section_min = section_max

    def destroy(self):
        self.pause()
        for piece in self.pieces:
            piece.destroy()
        for node in self.section_nodes:
            node.unlink()
        self.board.destroy()
        self.node.unlink()
        self.board_rect.unlink()
        self.section_node.unlink()
        self.starting_zone_node.unlink()

    def run(self):
        self.pause()
        self.interval_id = set_interval(self.tick_interval, self.tick)

    def pause(self):
        if self.interval_id:
            clear_interval(self.interval_id)
            self.interval_id = None

    def tick(self):
        """Advance the game by one step."""
        self.fall_and_freeze()
        self.delete_dissolving()
        self.mark_dissolving()
        if (not list(self.dissolving.get_blocks()) and
            self.board.frozen_grid.overlaps_any(self.starting_zone, (0, 0))):
            self.app.end_level()
            return

        self.ticks_to_next_piece -= 1
        if (self.ticks_to_next_piece <= 0 or
            not list(self.board.piece_grid.get_blocks())):
            self.ticks_to_next_piece = self.ticks_per_piece
            if not self.add_piece():
                self.app.end_level()
                return

        self.app.tick()

    def fall_and_freeze(self):
        """Classify all pieces as suspended (by a player grab), stopped (by
        frozen blocks), or falling; move falling pieces down by one step and
        freeze all stopped pieces."""

        # 1. Determine what supports each piece.  Pieces can be supported by
        # other pieces, by frozen blocks, or by the bottom edge of the board.
        supported_pieces = dict((piece, set()) for piece in self.pieces)
        stopped = set()
        for piece in self.pieces:
            for cr, value in piece.get_blocks():
                cr_below = add_cr(cr, (0, 1))
                if not self.board.piece_grid.in_bounds(cr_below):
                    stopped.add(piece)  # at bottom edge
                else:
                    piece_below = self.board.piece_grid.get(cr_below)
                    frozen_below = self.board.frozen_grid.get(cr_below)
                    if piece_below:  # supported by a Piece
                        supported_pieces[piece_below].add(piece)
                    elif frozen_below:  # supported by a frozen block
                        stopped.add(piece)

        # 2. Stop pieces supported by the board, and pieces they support.
        stopped = transitive_closure(stopped, supported_pieces)

        # 3. Suspend pieces that are grabbed, and pieces they support.
        suspended = set(piece for piece in self.pieces if piece.is_grabbed())
        suspended = transitive_closure(suspended, supported_pieces)

        # 4. Freeze all stopped pieces.
        for piece in stopped:
            if piece not in suspended:
                self.board.freeze_piece(piece)
                self.pieces.remove(piece)

        # 5. Move down all falling pieces.
        for piece in self.pieces:
            if piece not in suspended:
                piece.move_to(add_cr(piece.cr, (0, 1)))

    def delete_dissolving(self):
        """Delete any frozen blocks that are dissolving."""
        self.board.delete_frozen(self.dissolving)
        self.dissolving.clear()

    def mark_dissolving(self):
        """Mark dissolving blocks (to be deleted on the next tick)."""
        for section in self.sections:
            if self.board.frozen_grid.overlaps_all(section, (0, 0)):
                self.dissolving.put_all(section, (0, 0))
        self.board.highlight_dissolving(self.dissolving)

    def add_piece(self, c=None):
        """Place a new Piece with a randomly selected shape at a random
        rotation and position somewhere along the top of the game board."""
        for attempt in range(10):
            grid = random.choice(PIECE_GRIDS).get_rotated(random.randrange(4))
            blocks = list(grid.get_blocks())
            if c is None:  # Choose a random position.
                min_c = min(c for (c, r), value in blocks)
                max_c = max(c for (c, r), value in blocks)
                c = random.randrange(-min_c, self.board.ncolumns - max_c)
            else:  # Add the piece at the requested position.
                c -= grid.ncolumns/2
            cr = (c, -min(r for (c, r), value in blocks))
            if self.board.can_put_piece(None, cr, grid):
                self.pieces.append(Piece(self.node, self.board, cr, grid))
                return True
        return False  # couldn't find a location after 10 tries

LEVELS = [
    ([4], 1500, 3),
    ([4], 1200, 3),
    ([4], 1200, 2),
    ([4], 900, 2),
    ([3], 1500, 3),
    ([3], 1200, 3),
    ([3], 900, 3),
    ([3], 1200, 2),
    ([3, 3, 3, 3, 4, 4, 4, 4], 1200, 2),
    ([3, 3, 3, 3, 4, 4, 4, 4], 1000, 2),
    ([3], 1000, 2),
    ([3], 800, 2),
    ([2], 1000, 2),
    ([2], 800, 2),
]

class Multitet(AVGApp):
    multitouch = True

    def init(self):
        self._parentNode.mediadir = getMediaDir(__file__)

        self.level = None
        self.size = self._parentNode.size
        width = self.size.x
        height = self.size.y

        self.background = create_node(self._parentNode, 'rect',
            size=self.size, fillcolor='000000', fillopacity=1)
        self.level_node = create_node(self._parentNode, 'div',
            pos=Point2D(20, 20), size=Point2D(width - 40, height - 40))
        self.game_over_node = create_node(self._parentNode, 'div')
        create_node(self.game_over_node, 'rect', pos=self.size*0.25,
            size=self.size*0.5, fillcolor='000000', fillopacity=0.7)
        create_node(self.game_over_node, 'words', text='Game over',
            pos=Point2D(width*0.5, height*0.35),
            fontsize=80, alignment='center')
        create_button(self.game_over_node, self.start_level, text='Play again',
            fontsize=40, alignment='left', pos=Point2D(width*0.3, height*0.65))
        create_button(self.game_over_node, self.quit, text='Exit',
            fontsize=40, alignment='right', pos=Point2D(width*0.7, height*0.65))

        self.start_level()

    def _enter(self):
        if not self.level:
            self.start_level()
        self.level.run()

    def _leave(self):
        self.level.pause()

    def start_level(self):
        if self.level:
            self.level.destroy()
            self.level = None
        self.game_over_node.unlink()
        self.level = Level(self.level_node, 40, self, *LEVELS[0])
        self.difficulty = 0
        self.ticks = 0
        self.exit_button = create_button(self._parentNode, self.leave,
            pos=Point2D(self.size.x/2, 10), text='EXIT', alignment='center')
        self.level.run()

    def end_level(self):
        self.level.pause()
        self.exit_button.delete()
        self.game_over_node.unlink()
        self._parentNode.appendChild(self.game_over_node)

    def tick(self):
        self.ticks += 1
        if self.ticks > 40:
            self.difficulty += 1
            self.ticks = 0
            if self.difficulty >= len(LEVELS):
                self.difficulty = len(LEVELS) - 1
            section_pattern, tick_interval, ticks_per_piece = \
                LEVELS[self.difficulty]
            self.level.set_section_pattern(section_pattern)
            self.level.tick_interval = tick_interval
            self.level.ticks_per_piece = ticks_per_piece
            self.level.pause()
            self.level.run()

            level_words = create_node(self.level_node, 'words',
                text='Level %d' % (self.difficulty + 1), alignment='center',
                pos=self.size/2, fontsize=80, sensitive=False)
            fadeOut(level_words, 2000)

    def quit(self):
        self.start_level()
        self.leave()

if __name__ == '__main__':
    Multitet.start(resolution=(1280, 720))
