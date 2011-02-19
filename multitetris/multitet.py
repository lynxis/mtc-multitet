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

from libavg import avg, Point2D, AVGApp, AVGNode, fadeOut, gameapp
from libavg.AVGAppUtil import getMediaDir
from buttons import LabelButton
from grid import Grid
import math, random

STARTING_ZONE_NROWS = 1  # size of the starting zone, in rows

MANIPULATOR_RADIUS = 140  # radius of the circular manipulator, in pixels

TICKS_PER_LEVEL = 40

def make_cells(str):
    return [[ch != '-' and ch or None for ch in row] for row in str.split()]

NORMAL_SHAPES = [
    Grid(4, 4, make_cells('---- -FF- -F-- -F--')),  # F-shape
    Grid(4, 4, make_cells('-L-- -L-- -LL- ----')),  # L-shape
    Grid(4, 4, make_cells('--I- --I- --I- --I-')),  # I-shape
    Grid(3, 3, make_cells('-SS SS- ---')),  # S-shape
    Grid(3, 3, make_cells('ZZ- -ZZ ---')),  # Z-shape
    Grid(3, 3, make_cells('--- TTT -T-')),  # T-shape
    Grid(2, 2, make_cells('XX XX')),  # X-shape
]

BIG_SHAPES = [
    Grid(3, 3, make_cells('XXX X-- X--')),
    Grid(5, 5, make_cells('--I-- --I-- --I-- --I-- --I--')),
    Grid(3, 3, make_cells('-SS -SS -S-')),
    Grid(3, 3, make_cells('ZZ- ZZ- -Z-')),
    Grid(5, 5, make_cells('--L-- --L-- --L-- --LL- -----')),
    Grid(5, 5, make_cells('----- --FF- --F-- --F-- --F--')),
]

TOUGH_SHAPES = [
    Grid(3, 3, make_cells('TTT -T- -T-')),
    Grid(4, 4, make_cells('-S-- -S-- -SS- --S-')),
    Grid(4, 4, make_cells('--Z- --Z- -ZZ- -Z--')),
    Grid(4, 4, make_cells('-F-- -FF- -F-- -F--')),
    Grid(4, 4, make_cells('--L- -LL- --L- --L-')),
    Grid(3, 3, make_cells('--- X-X XXX')),
]

AWFUL_SHAPES = [
    Grid(3, 3, make_cells('-X- XXX -X-')),
    Grid(3, 3, make_cells('LL- -LL -L-')),
    Grid(3, 3, make_cells('-FF FF- -F-')),
    Grid(3, 3, make_cells('-SS -S- SS-')),
    Grid(3, 3, make_cells('ZZ- -Z- -ZZ')),
    Grid(3, 3, make_cells('II- -II --I')),
]

EASY_SHAPES = [
    Grid(2, 2, make_cells('XX X-')),
    Grid(3, 3, make_cells('-I- -I- -I-')),
]

TINY_SHAPES = [
    Grid(2, 2, make_cells('II --')),
    Grid(1, 1, make_cells('X')),
]


# As levels progress, the scale must not increase (the board cannot shrink)!
LEVELS = [
    # scale, sections, tick_interval, ticks_per_drop, pieces_per_drop, shapes
    (40, [4], 1500, 3, 1, NORMAL_SHAPES),  # Normal
    (40, [4], 1200, 2, 1, NORMAL_SHAPES),  # Faster
    (40, [4], 1200, 6, 3, NORMAL_SHAPES),  # Crazy round!
    (40, [3], 1200, 2, 1, NORMAL_SHAPES),  # Back to normal...
    (40, [3], 1000, 6, 3, NORMAL_SHAPES),

    # Scale up the board!
    (34, [4], 1200, 2, 1, NORMAL_SHAPES),
    (34, [4], 1000, 4, 2, NORMAL_SHAPES),
    (34, [4], 1200, 6, 4, NORMAL_SHAPES),
    (34, [3], 1200, 4, 2, NORMAL_SHAPES),
    (34, [3], 1000, 6, 4, NORMAL_SHAPES),

    # Introduce some new shapes.
    (34, [4], 1400, 2, 1, NORMAL_SHAPES*3 + BIG_SHAPES*2),
    (34, [4], 1200, 3, 2, NORMAL_SHAPES*3 + BIG_SHAPES*2),
    (34, [4], 1000, 6, 3, NORMAL_SHAPES*3 + BIG_SHAPES*2),
    # Scale up again!  Take one level to recover.
    (30, [4], 1500, 3, 2, NORMAL_SHAPES),
    (30, [4], 1200, 6, 5, NORMAL_SHAPES), # Crazy again!

    # A higher proportion of big shapes.
    (30, [4], 1500, 3, 2, NORMAL_SHAPES*2 + BIG_SHAPES*3),
    (30, [4], 1200, 3, 2, NORMAL_SHAPES*2 + BIG_SHAPES + TOUGH_SHAPES),
    # More new shapes!  Add some easy ones to make up for the hard ones.
    (30, [4], 1200, 6, 4, NORMAL_SHAPES*2 + BIG_SHAPES + TOUGH_SHAPES +
                          EASY_SHAPES*6),
    (30, [4], 1200, 3, 2, NORMAL_SHAPES*3 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES + EASY_SHAPES*6 + TINY_SHAPES*6),
    (30, [4], 1200, 6, 4, NORMAL_SHAPES*3 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES + EASY_SHAPES*12 + TINY_SHAPES*12),

    # Lightning round!  Fast, but easier.
    (30, [4], 800, 2, 1, NORMAL_SHAPES + EASY_SHAPES*6 + TINY_SHAPES*3),
    (30, [4], 1200, 6, 6, NORMAL_SHAPES + EASY_SHAPES*6 + TINY_SHAPES*3),
    (30, [4], 800, 6, 6, EASY_SHAPES*2 + TINY_SHAPES),
    # Scale up again.  No easy shapes.
    (26, [4], 1400, 3, 2, NORMAL_SHAPES*3 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES),
    (26, [4], 1200, 6, 5, NORMAL_SHAPES*2 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES),

    # Go nuts with the horrible shapes!
    (26, [4], 1500, 3, 2, NORMAL_SHAPES*3 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES + EASY_SHAPES*6 + TINY_SHAPES*6),
    (26, [4], 1000, 3, 2, NORMAL_SHAPES*2 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES + EASY_SHAPES*6 + TINY_SHAPES*6),
    (26, [4], 1200, 6, 4, NORMAL_SHAPES*2 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES + EASY_SHAPES*6 + TINY_SHAPES*6),
    (26, [4], 1200, 6, 6, NORMAL_SHAPES*2 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES + EASY_SHAPES*6 + TINY_SHAPES*6),
    (26, [4], 1000, 6, 6, NORMAL_SHAPES*2 + BIG_SHAPES + TOUGH_SHAPES +
                          AWFUL_SHAPES + EASY_SHAPES*6 + TINY_SHAPES*6),
]

def set_handler(node, type, handler):
    node.setEventHandler(type, avg.TOUCH, handler)

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

def get_vector(heading, radius):
    """Get the vector with the given heading in degrees (where 0 is north
    and 90 is east) and the given radius."""
    degrees = 90 - heading
    radians = degrees*math.pi/180
    return Point2D(radius*math.cos(radians), -radius*math.sin(radians))

def get_length(origin, point):
    vector = point - origin
    return (vector.x*vector.x + vector.y*vector.y)**0.5

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

def shuffled(items):
    new_items = items[:]
    random.shuffle(new_items)
    return new_items

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
        self.grab_center = None  # Center point of this piece when grabbed.
        self.grab_angle = None  # Heading from grab_centroid to grab_center.
        self.grab_radius = None  # Distance from grab_centroid to grab_center.
        self.grab_cr = None  # Grid position of this piece when grabbed.

        self.grab_heading = None  # Angle of ray between initial grab points.
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
            r=MANIPULATOR_RADIUS, color='000000', opacity=0,
            fillcolor='ffffff', fillopacity=0)
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

    def get_center_point(self):
        """Get the center point of this piece, in absolute coordinates."""
        nw_cr = self.cr
        se_cr = add_cr(nw_cr, (self.grid.ncolumns, self.grid.nrows))
        board_point = get_centroid(map(self.board.get_nw_point, [nw_cr, se_cr]))
        return self.board.parent_node.getAbsPos(board_point)

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
        self.grab_center = self.get_center_point()
        self.grab_centroid = get_centroid(self.touches.values())
        self.grab_angle = get_heading(self.grab_centroid, self.grab_center)
        self.grab_radius = get_length(self.grab_centroid, self.grab_center)
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
            self.grab_center = self.get_center_point()
            self.grab_centroid = get_centroid(self.touches.values())
            self.grab_angle = get_heading(self.grab_centroid, self.grab_center)
            self.grab_radius = get_length(self.grab_centroid, self.grab_center)
            self.grab_cr = self.cr

        self.update_manip()

    def handle_motion(self, event):
        if event.cursorid not in self.touches:  # Ignore extraneous touches.
            return

        self.touches[event.cursorid] = event.pos

        # If there are two touches, update rotation.
        if len(self.touches) == 2:
            new_heading = get_heading(*self.touches.values())
            delta_heading = new_heading - self.grab_heading
            cw_quarter_turns = int(round(delta_heading/90))
            new_grid = self.grab_grid.get_rotated(cw_quarter_turns)
        else:
            delta_heading = 0
            new_grid = self.grid

        # Update translation.  The center of the piece is positioned at a
        # fixed distance from the centroid of the grab points, and at a
        # fixed angle with respect to the line between the grab points.
        # This creates the effect of grabbing a rigid disk; if the grab
        # points stay the same distance apart, the two points on the piece
        # that were initially grabbed will be anchored to the touch points.
        new_centroid = get_centroid(self.touches.values())
        new_center = new_centroid + get_vector(
            self.grab_angle + delta_heading, self.grab_radius)
        delta_center = new_center - self.grab_center
        delta_c = int(round(delta_center.x/self.board.scale))
        delta_r = int(round(delta_center.y/self.board.scale))
        new_cr = add_cr(self.grab_cr, (delta_c, delta_r))

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
        self.piece_grid = None  # set_size will set these
        self.frozen_grid = None
        self.set_size(ncolumns, nrows, pos, scale)

    def set_size(self, ncolumns, nrows, pos, scale):
        self.ncolumns = ncolumns
        self.nrows = nrows
        self.pos = pos
        self.scale = float(scale)
        self.size = Point2D(ncolumns, nrows)*self.scale
        self.block_size = Point2D(self.scale, self.scale)
        if self.piece_grid:
            row_offset = nrows - self.piece_grid.nrows
            self.piece_grid.grow(ncolumns, nrows)
            self.frozen_grid.grow(ncolumns, nrows)
            pieces = set(value for cr, value in self.piece_grid.get_blocks())
            for piece in pieces:
                c, r = piece.cr
                piece.cr = (c, r + row_offset)
                piece.update_blocks()
                piece.update_manip()
            for cr, value in self.frozen_grid.get_blocks():
                value.pos = self.get_nw_point(cr)
                value.size = self.block_size
        else:
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

class Game:
    """The main game controller.  This class holds the Board and the list of
    Pieces, and causes the Pieces to fall with each clock tick.  When the fall
    of a Piece is stopped by blocks below, the Piece "freezes": the blocks of
    the Piece are copied onto the board and the Piece object is destroyed.
    Pieces appear in a "starting zone" at the top where they cannot be
    manipulated; if a piece freezes in the starting zone, the game ends."""

    def __init__(self, parent_node, app, scale, section_pattern,
                 tick_interval, ticks_per_drop, pieces_per_drop, shapes):
        self.width, self.height = parent_node.size.x, parent_node.size.y
        self.app = app

        self.node = create_node(parent_node, 'div')
        self.section_node = create_node(self.node, 'div', sensitive=False)

        # Create a dummy game board; it will be resized in self.set_scale().
        self.board = Board(self.node, 1, 1, Point2D(0, 0), scale)
        self.board_rect = create_node(self.node, 'rect',
            color='a0a0a0', strokewidth=4, sensitive=False)
        self.dissolving = None

        self.starting_zone = None
        self.starting_zone_node = create_node(parent_node, 'rect',
            opacity=0, fillcolor='ff0000', fillopacity=0.2)
        self.can_add_pieces = True  # For debouncing taps on the starting zone.

        # Normally, the starting zone can be touched to request more pieces;
        # however, this feature is disabled because of an exhaust hose in the
        # c-base multitouch table that causes extraneous touches along the top.
        # set_handler(self.starting_zone_node, avg.CURSORDOWN, self.handle_down)

        self.pieces = []
        self.interval_id = None
        self.ticks_to_next_drop = 1
        self.section_nodes = None
        self.set_difficulty(scale, section_pattern, tick_interval,
                            ticks_per_drop, pieces_per_drop, shapes)

    def set_difficulty(self, scale, section_pattern, tick_interval,
                       ticks_per_drop, pieces_per_drop, shapes):
        self.set_scale(scale)
        self.set_section_pattern(section_pattern)
        self.tick_interval = tick_interval
        self.ticks_per_drop = ticks_per_drop
        self.pieces_per_drop = pieces_per_drop
        self.shapes = shapes

    def set_scale(self, scale):
        # Resize the game board.
        ncolumns, nrows = int(self.width/scale), int(self.height/scale)
        board_width = ncolumns*scale
        self.board.set_size(ncolumns, nrows,
            Point2D((self.width - board_width)/2, self.height % scale), scale)
        self.board_rect.pos = self.board.pos
        self.board_rect.size = self.board.size
        if self.dissolving:
            self.dissolving.grow(self.board.ncolumns, self.board.nrows)
        else:
            self.dissolving = Grid(self.board.ncolumns, self.board.nrows)

        # Set up the starting zone.
        self.starting_zone = Grid(self.board.ncolumns, STARTING_ZONE_NROWS,
            ['X'*self.board.ncolumns]*STARTING_ZONE_NROWS)
        self.starting_zone_node.pos = self.board.pos
        self.starting_zone_node.size = \
            Point2D(self.board.ncolumns, STARTING_ZONE_NROWS)*scale

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
        if self.section_nodes:
            for node in self.section_nodes:
                node.unlink()

        self.section_nodes = []
        self.sections = []
        s = 0

        for r in reversed(range(STARTING_ZONE_NROWS, self.board.nrows)):
            nsections = section_pattern[s]
            s = (s + 1) % len(section_pattern)

            # If the resolution is too low, 4 sections are no fun.
            if self.width < 1200 and nsections > 3:
                nsections = 3

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
            self.app.end_game()
            return

        self.ticks_to_next_drop -= 1
        if (self.ticks_to_next_drop <= 0 or
            not list(self.board.piece_grid.get_blocks())):
            self.ticks_to_next_drop = self.ticks_per_drop
            for p in range(self.pieces_per_drop):
                if not self.add_piece():
                    print 'end_game: add_piece failed'
                    self.app.end_game()
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
        columns = (c is None) and range(self.board.ncolumns) or [c]

        # Permute the shapes, rotations, and possible positions, so that we
        # get a random result, but eventually try them all.
        for shape in shuffled(self.shapes):
            for rotation in shuffled(range(4)):
                for c in shuffled(columns):
                    grid = shape.get_rotated(rotation)
                    min_r = min(r for (c, r), value in grid.get_blocks())
                    cr = (c - grid.ncolumns/2, -min_r)
                    if self.board.can_put_piece(None, cr, grid):
                        self.pieces.append(
                            Piece(self.node, self.board, cr, grid))
                        return True
        return False  # couldn't find anywhere to put a new piece

class Multitet(gameapp.GameApp):

    def init(self):
        self._parentNode.mediadir = getMediaDir(__file__)

        self.game = None
        self.size = self._parentNode.size
        self.text_size = 20
        self.text_line = Point2D(0, self.text_size)
        self.margin = Point2D(20, 20)

        self.background = create_node(self._parentNode, 'rect',
            size=self.size, fillcolor='000000', fillopacity=1)
        self.game_node = create_node(self._parentNode, 'div',
            pos=self.margin + self.text_line/2,
            size=self.size - self.margin*2 - self.text_line/2)
        self.create_button(
            self._parentNode, self.show_about_box, 'About', 1,
            self.margin - self.text_line/2, 'left')
        self.level_button_nodes = []

        self.game_over_box = self.create_game_over_box()
        self.about_box = self.create_about_box()
        self.start_game()

    def create_words(self, parent, text, scale=1.0, alignment='left', **props):
        return create_node(parent, 'words', text=text,
            fontsize=self.text_size*scale, alignment=alignment, **props)

    def create_button(self, parent_node, callback, text, scale, pos, alignment):
        padding = Point2D(self.text_size*0.4, self.text_size*0.2)
        button = LabelButton(
            parent_node, pos, text, self.text_size*scale, callback)
        button._node.alignment = alignment
        if alignment == 'left':
            pos = button._node.pos - padding
        if alignment == 'center':
            pos = button._node.pos - Point2D(button._node.size.x/2, 0) - padding
        if alignment == 'right':
            pos = button._node.pos - Point2D(button._node.size.x, 0) - padding
        button_box = create_node(parent_node, 'rect', pos=pos,
            size=button._node.size + padding*2, color='ffffff',
            fillcolor='808080', fillopacity=0.75)
        parent_node.removeChild(button_box)
        parent_node.insertChildBefore(button_box, button._node)
        return [button._node, button_box]

    def create_game_over_box(self):
        box = create_node(self._parentNode, 'div')
        create_node(box, 'rect', fillcolor='404040', fillopacity=0.75,
            pos=self.get_pos(0.5, 0.5, -self.text_size*9, -self.text_size*6),
            size=Point2D(self.text_size*18, self.text_size*12))
        self.create_words(box, 'Game over', 3, alignment='center',
            pos=self.get_pos(0.5, 0.5, 0, -self.text_size*5))
        self.create_button(box, self.start_game, 'Play again', 1.6,
            self.get_pos(0.5, 0.5, -self.text_size*7, self.text_size*3), 'left')
        self.create_button(box, self.quit, 'Exit', 1.6,
            self.get_pos(0.5, 0.5, self.text_size*7, self.text_size*3), 'right')
        return box

    def create_about_box(self):
        box = create_node(self._parentNode, 'div')
        create_node(box, 'rect', fillcolor='404040', fillopacity=0.75,
            pos=self.get_pos(0.5, 0.5, -self.text_size*15, -self.text_size*10),
            size=Point2D(self.text_size*30, self.text_size*20))
        y = -9*self.text_size
        for scale, line in [
            (1.6, 'Multitetris'),
            (1, 'Ka-Ping Yee, Martin Heistermann, Ulrich von Zadow'),
            (1, 'http://multitetris.com/'),
            (1, ''),
            (1, 'Use one finger to move a piece.'),
            (1, 'Use a second finger to rotate a piece.'),
            (1, 'You can hold pieces in the air, or even push them up.'),
            (1, 'But watch out: new pieces will keep coming!'),
            (1, ''),
            (1, 'The rows are divided into sections.  When a section of'),
            (1, 'one row is filled with blocks, those blocks disappear.'),
            (1, 'Keep them from piling up to the top as long as you can.')]:
            self.create_words(box, line, scale,
                pos=self.get_pos(0.5, 0.5, -self.text_size*14, y))
            y += self.text_size*scale*1.2
        self.create_button(box, self.hide_about_box, 'Continue', 1.6,
            self.get_pos(0.5, 0.5, 0, self.text_size*7), 'center')
        return box

    def get_pos(self, fraction_x, fraction_y, offset_x=0, offset_y=0):
        return Point2D(self.size.x*fraction_x + offset_x,
            self.size.y*fraction_y + offset_y)

    def _enter(self):
        if not self.game:
            self.start_game()
        self.game.run()

    def _leave(self):
        self.game.pause()

    def show(self, node):
        node.unlink()
        self._parentNode.appendChild(node)

    def hide(self, node):
        node.unlink()

    def show_about_box(self):
        self.game.pause()
        self.show(self.about_box)

    def hide_about_box(self):
        self.hide(self.about_box)
        self.game.run()

    def start_game(self):
        self.hide(self.about_box)
        self.hide(self.game_over_box)
        if self.game:
            self.game.destroy()
        self.game = Game(self.game_node, self, *LEVELS[0])
        self.ticks_to_next_level = TICKS_PER_LEVEL
        self.set_level(1)

    def end_game(self):
        self.game.pause()
        self.show(self.game_over_box)

    def set_level(self, level):
        self.level = min(level, len(LEVELS))
        self.game.set_difficulty(*LEVELS[self.level - 1])  # level is 1-based
        self.game.run()

        level_words = self.create_words(
            self._parentNode, 'Level %d  ' % self.level, 3,
            pos=self.get_pos(0.5, 0.3), alignment='center', sensitive=False)
        fadeOut(level_words, 2000)

        for node in self.level_button_nodes:
            node.unlink()
        self.level_button_nodes = self.create_button(
            self._parentNode, self.advance_level, 'Level %d' % self.level, 1,
            Point2D(self.size.x - self.margin.x,
                self.margin.y - self.text_size/2), 'right')

    def advance_level(self):
        self.set_level(self.level + 1)
        self.ticks_to_next_level = TICKS_PER_LEVEL

    def onKeyDown(self, event):
        if event.keystring == 'l':
            self.advance_level()
        if event.keystring == 'q':
            self.end_game()
        if event.keystring == ' ':
            self.show_about_box()

    def tick(self):
        self.ticks_to_next_level -= 1
        if self.ticks_to_next_level <= 0:
            self.set_level(self.level + 1)
            self.ticks_to_next_level = TICKS_PER_LEVEL

    def quit(self):
        avg.Player.get().stop()
        self.exit()

if __name__ == '__main__':
    Multitet.start()
