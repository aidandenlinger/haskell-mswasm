## Wasm .wat -> MS-Wasm .wat Conversion Brainstorming

### Things to Convert
- load/store -> segment load/store
    - we have all load/store equivalents except for floats

- add handles as arguments/replace i32 arguments
    - new prelude/postlude - need to *define* that handle (coarse-grained for
      this scripting approach)
        - new local, init @ top of module to max used of memory (`countVars`
          problem)
            - start w/ lazy approach (large number), later have it passed in
              as an argument?
        - also free the handle at the end because we are good memory consumers
    
        - *actual representation*: add an extra local Handle to each
          function, which gets referred to by index
        - add at beginning of instructions a create segment with large memory
          number (change to user parameter at some point)
        - add free handle to the end
    
    - given a handle and a load/store, take `<addr>` -> `(handle.set_offset
    (get_local $h) <addr>)`
        - `<addr>` is any linear memory address, e.g. `(i32.const <val>) or
          (get_local <index>)`
        - problem is knowing what those addrs are/when they come about

        - *actual representation*: run through function list, pattern match
        on loads and stores?
            - do we need to manage the stack :( if params are pushed onto the
              stack way earlier, we need to map params to their function calls
              such that we actually replace the right params
            - look into `Validate.hs` as an entry point? looks like it doesn't
              actually check locals :(

- arguments in linear memory/data initialization?
    - can't *handle* it lmao
    - the way this would work, we can't directly access linear memory
    - for now, we'd need to require each module is self-contained or doesn't
      grab from linear memory external from this function
    - data initalization would also be problematic
    - could add logic for function -> function stuff, but error prone and not
      part of MVP