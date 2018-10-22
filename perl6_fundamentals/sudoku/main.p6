use v6;

my $sudoku = '000000075000080094000500600010000200000900057006003040001000023080000006063240000';
$sudoku .= trans('0' => ' ');

sub chunks(Str $s, Int $chars) {
    gather loop (my $j = 0; $j < $s.chars; $j += $chars) {
        take substr $s, $j, $chars;
    }
}

my $row_num = 0;
my $separator = "+---+---+---+";
for chunks($sudoku, 9) -> $row {
    if $row_num++ %% 3 {
        say $separator;
    }
    say "|", chunks($row, 3).join("|"), "|";
}
say $separator;
