
import System.Process.Extra

main = do
    system_ "curl -sSL https://get.haskellstack.org/ | sh"
    system_ "stack init --resolver=nightly --ignore-subdirs --force"
    system_ "weeder . --build --verbose +RTS -K1K"
    system_ "weeder --test --verbose +RTS -K1K"
