exportShLib () {
    export SH_LIB="@out@/lib"
}

addEnvHooks "$hostOffset" exportShLib
