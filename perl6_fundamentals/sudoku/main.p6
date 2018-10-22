use v6;

my $sudoku = '000000075000080094000500600010000200000900057006003040001000023080000006063240000';
$sudoku .= trans: '0' => ' ';

constant $separator = "+---+---+---+";
for $sudoku.comb(9) -> $row {
    say $separator if $++ %% 3;
    say "|", $row.comb(3).join("|"), "|";
}
say $separator;
