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
