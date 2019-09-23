import os
import os.path
import saw

dir_path = os.path.dirname(os.path.realpath(__file__))


c = saw.connect("cabal new-exec saw-remote-api -- --dynamic4")
c.cryptol_start_setup("x")
d = c.snapshot()


d.cryptol_finish_setup().result()

c.cryptol_load_file(os.path.join(dir_path, 'Foo.cry'))
c.cryptol_finish_setup().result()
