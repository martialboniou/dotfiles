# /usr/bin/env gawk -f
# reduces leading spaces by half
# goal: fix `gawk -o` formatting w/o options
{
    match($0, /^ */)
    if (RLENGTH == 0) {
        print
    } else {
        s = ""
        ns = RLENGTH / 2
        for (i = 1; i <= ns; i++) {
            s = s " "
        }
        new = sprintf("%s%s", s, substr($0, RLENGTH + 1))
        print new
    }
}
