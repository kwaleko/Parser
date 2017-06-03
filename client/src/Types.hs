module App.Posts where
  
import App.Post as P
import Control.Monad.Aff (attempt)
import DOM (DOM)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Array (snoc, filter)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX, get, post, delete)
import Prelude (bind, ($), (<>), show, (<<<), pure, map, const, (/=))
import Pux (EffModel, noEffects)
import Pux.Html (Html, div, h1, text, ol, li, h2, button)
import Pux.Html.Attributes (key, className, type_)
import Pux.Html.Events (onClick)
import Pux.Router (link)


newType Post = 
    Post 
    {
        id     :: Maybe Int
        ,title :: String
        ,body  :: String
    }

type Posts = Array Post

type State = Posts

data Action = 
    RequestPosts 
    -- | CreatePost
    | ReceivePosts (Either String Posts)   
    -- | DeletePost Int 

init :: State
init =  [] 

update :: Action -> State -> EffModel State Action (dom :: DOM, ajax :: AJAX)
update (ReceivePosts (Left err)) state =
  noEffects $ state { status = "Error fetching posts: " <> show err }
update (ReceivePosts (Right posts)) state =
  noEffects $ state { posts = posts, status = "Posts" }
update (RequestPosts) state =
  { state: state { status = "Fetching posts..." }
  , effects: [ do
      res <- attempt $ get "http://localhost:3000/Articles"
      let decode r = decodeJson r.response :: Either String Posts
      let posts = either (Left <<< show) decode res
      pure $ ReceivePosts posts
    ]
  }

  view :: State -> Html Action
view state =
  div []
    [ h1 [] [ text state.status ]
    , button [ type_ "button", onClick (const CreatePost) ] [ text "Create new post" ]
    , ol [] $ map postView state.posts
    ]

postView :: Post -> Html Action
postView (P.Post p) =
  case p.id of
    Nothing -> li [] []
    (Just id) -> li [ key (show id), className "post" ]
                 [ h2 []
                   [ link ("/posts/" <> show id) []
                     [ text p.title ]
                   ]
                 , button [ type_ "submit", onClick (const $ DeletePost id) ] [ text "Delete" ]
                 ]