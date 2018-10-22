#!/usr/bin/env perl6
# this is fine, but the type conversion below is finer.
# sub MAIN(Int $timestamp) {
#     my $dt = DateTime.new(+$timestamp);
#     if none $dt.hour, $dt.minute, $dt.second {
#         say $dt.Date;
#     }
#     else {
#         say $dt;
#     }
# }

# better solution based on type conversions
sub MAIN(Int $timestamp) {
    my $dt = DateTime.new(+$timestamp);

    if $dt.Date.DateTime == $dt {
        say $dt.Date;
    }
    else {
        say $dt;
    }
}
