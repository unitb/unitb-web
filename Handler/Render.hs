module Handler.Render where

import           Control.Lens
import qualified Data.Vector as V
import           Logic.Expr hiding ((</>),Value,render)
import           Logic.Proof.Monad
import           System.Directory
import           System.FilePath
import           TeX2PNG
import           Utilities.Syntactic (Error, show_err)

import           Import hiding ((.=))
import           Logic.Prover
import           Logic.Utilities
import           Model.ProofForm

postRenderR :: Handler Value
postRenderR = do
  cur <- lift getCurrentDirectory  -- project root
  let staticSubDir = "img"
      imgDir = cur </> "static" </> staticSubDir
  content <- requireJsonBody :: Handler (Text)
  path <- lift $ render (mkPNG (id .= args imgDir) content) staticSubDir
  returnJson path
  where
    args d = Args SimplyTransparent (Just d) 150 False True Nothing 1 packages Nothing True
    packages = pack <$> ["bsymb", "eventB", "unitb", "calculational"]


postFormPngR :: Handler Value
postFormPngR = do
  form <- requireJsonBody :: Handler (ProofForm Text)
  path <- lift $ renderForm form mkPNG True "img"
  returnJson path

postFormPdfR :: Handler Value
postFormPdfR = do
  form <- requireJsonBody :: Handler (ProofForm Text)
  path <- lift $ renderForm form mkPDF False "pdf"
  returnJson path


renderForm :: ProofForm Text
           -> (OptArgs () -> Text -> IO (Either Text FilePath))
           -> Bool
           -> FilePath
           -> IO FilePath
renderForm proofForm fun tightness staticSubDir = do
  cur <- getCurrentDirectory  -- project root
  let imgDir = cur </> "static" </> staticSubDir
  path <- render (fun (id .= args imgDir tightness) (getLatex proofForm)) staticSubDir
  return path
  where
    args d t = Args SimplyTransparent (Just d) 150 False True Nothing 1 packages Nothing t
    packages = pack <$> ["bsymb", "eventB", "unitb", "calculational"]
    getLatex :: ProofForm Text -> Text
    getLatex form = intercalate "\n"
                    [ begin
                    , "\\\\[-2.5pt]"
                    , "\\vdash & \\\\[-2.5pt]"
                    , "& " <> goal'
                    , end
                    ]
      where
        begin,end,theoriesDeclsBox :: Text
        begin = concat [ "\\begin{array}{r@{~}lr}"
                       , theoriesDeclsBox
                       , assumptions'
                       ]
        end = "\\end{array}"
        theoriesDeclsBox = if (null theories') && (null declarations')
              then ""
              else intercalate "\n"
                   [ "& \\boxed{"
                   , "\\begin{array}{l}"
                   , concat
                     [ theories'
                     , declarations'
                     ]
                   , "\\end{array}"
                   , "} \\\\"
                   ]
        theories',declarations',assumptions',goal' :: Text
        theories' = if null ts
                    then ""
                    else intercalate "\n"
                         [ "\\textsf{using} \\\\"
                         , "\\quad " <> "\\textit{" <> ts <> "} \\\\"
                         ]
          where ts = intercalate ", " $ pack <$> form^.theories
        declarations' = if null ds
                        then ""
                        else intercalate "\n"
                             [ "\\textsf{constants} \\\\"
                             , "\\quad " <> ds]
          where
            ds = intercalate " \\\\\n\\quad " $
                 intercalate "\n" <$>
                 decls (form^.theories) (form^.declarations)
        assumptions' = if null as
                       then ""
                       else "\\\\" <> as
          where
            as = assums $ form^.assumptions
        goal' = form^.goal

    decls :: Vector String -> Vector (String, Text) -> [[Text]]
    decls ts ds = do
      let parsedDecls :: [Either [Error] [(Name,Var)]]
          parsedDecls = V.toList $ parseDeclarations (toList $ getTheories ts)
                        <$> (\(declName, decl) -> toStringLi declName (unpack decl))
                        <$> ds
      runDecl <$> parsedDecls
      where
        runDecl = either (\errs -> [pack $ show_err errs]) (fmap (pack . varDecl . snd))

    assums :: Vector (String, (String, Text)) -> Text
    assums = intercalate asmsep . map oneLine . toList
    asmsep = " \\\\\n"
    oneLine (_, (lbl, asm)) = concat
                              ["& ", asm, " & \\textsf{(", pack lbl, ")}"]


render :: IO (Either Text FilePath)
       -> FilePath
       -> IO FilePath
render cmd outputDir = do
  let outPath = "static" </> outputDir
  file <- cmd
  let fileFullPath = either unpack id file
  return $ outPath </> (takeFileName fileFullPath)
