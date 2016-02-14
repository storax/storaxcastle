function zaw-src-aliases() {
    title="aliases"
    desc="$(alias | zaw-src-aliases-desc)"
    units="$(alias | cut -d '=' -f 1)"
    : ${(A)candidates::=${(f)units}}
    : ${(A)cand_descriptions::=${(f)desc}}
    actions=(zaw-callback-execute zaw-callback-append-to-buffer zaw-callback-replace-buffer view-alias-pager)
    act_descriptions=("execute" "append to buffer" "replace buffer" "view alias")
    options=(-t "$title")
}

zaw-src-aliases-max() {
    alias | awk -F'=' '{max=(length($1)>max?length($1):max)} END {print max}'
}

zaw-src-aliases-desc() {
    max=`alias | awk -F'=' '{max=(length($1)>max?length($1):max)} END {print max}'`
    awk -F'=' -v max=$max 'BEGIN {format = "%-" max "s = "} {printf (format, $1); for (i=2; i<NF; i++) printf $i "="; print $NF}'
}

view-alias-pager() {
    alias $1 | ${PAGER:-less}
}

zaw-register-src -n aliases zaw-src-aliases
