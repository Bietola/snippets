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

sub from-time-stamp(Int \timestamp) {
    sub formatter($_) {
        sprintf '%04d-%02d-%02d %02d:%02d:%02d',
                .year, .month, .day,
                .hour, .minute, .second;
    }

    given DateTime.new(+timestamp, :&formatter) {
        if .Date.DateTime == $_ {
            say .Date;
        }
        else {
            .say
        }
    }
}

sub from-date-string(Str $date, Str $time?) {
    my $dt = Date.new($date);
    if $time {
        my ($hour, $minute, $second) = $time.split(":");
        say DateTime.new(date => $dt, :$hour, :$minute, :$second).posix;
    }
    else {
        say $dt.DateTime.posix;
    }
}

# convert a timestamp to a date
multi sub MAIN(Int \timestamp) {
    from-time-stamp(timestamp);
}

# convert from date to timestamp
multi sub MAIN(Str $date where { try Date.new($_) }, Str $time?) {
    from-date-string($date, $time);
}
