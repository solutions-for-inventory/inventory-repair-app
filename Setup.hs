import Distribution.PackageDescription (PackageDescription(extraSrcFiles))
import Distribution.Simple (UserHooks(buildHook))
import qualified Distribution.Simple as DS
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags)
import qualified System.Process as Proc
import qualified System.Directory as Dir
import Control.Monad (when, void)

buildUI :: IO ()
buildUI = do
            distExists <- Dir.doesDirectoryExist "webapps/dist"
            () <- case distExists of
                    True -> Dir.removeDirectoryRecursive "webapps/dist"
                    False -> return ()
            () <- Dir.createDirectory "webapps/dist"
            Proc.system "cd ui-home && yarn install && yarn build"
            Proc.system "cp -a ui-home/build/. webapps/dist"
            return ()

buildHook_ :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildHook_ packageDesc localBuildInfo userHooks buildFlags = do
--    exists <- Dir.doesFileExist "./app/dist/app.bundle.js"
--    when (not exists) $ void $ Proc.system "cd app && npm install && npm run build-js"
--    () <- buildUI
--    print buildFlags
    (buildHook DS.simpleUserHooks) packageDesc localBuildInfo userHooks buildFlags

main :: IO ()
main = DS.defaultMainWithHooks DS.simpleUserHooks{buildHook = buildHook_}
-- https://stackoverflow.com/questions/38903177/how-to-run-bash-commands-like-npm-install-on-complie/39078496#39078496
-- https://github.com/nakaji-dayo/yesod-job-queue
