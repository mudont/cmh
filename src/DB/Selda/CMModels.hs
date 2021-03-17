module DB.Selda.CMModels where

import ClassyPrelude
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Database.Selda hiding (Group)
import Database.Selda.PostgreSQL ()

data SiteType = Cricket | Tennis | Exchange | Other
  deriving (Show, Read, Bounded, Enum)

instance SqlType SiteType

$(deriveJSON defaultOptions ''SiteType)


-- User
data User = User
  { id :: ID User,
    username :: Text,
    password :: Text,
    last_login :: Maybe UTCTime,
    first_name :: Text,
    last_name :: Text,
    email :: Maybe Text,
    is_staff :: Bool,
    is_active :: Bool,
    is_superuser :: Bool,
    email_verified :: Bool,
    date_joined :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow User

user :: Table User
user = table "auth_user" 
  [ #id :- autoPrimary
  , #username :- unique
  , #email :- unique
  ]

$(deriveJSON defaultOptions ''User)
data ResetToken = ResetToken
  { token :: Text,
    user_id :: ID User,
    expiration :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow ResetToken

resetToken :: Table ResetToken
resetToken = table "reset_token"
  [ #token :- primary
  , #user_id :- foreignKey user #id
  ]
$(deriveJSON defaultOptions ''ResetToken)

-- Player

data Player = Player
  { id :: ID Player,
    user_id :: ID User,
    mobile_phone :: Maybe Text,
    home_phone :: Maybe Text,
    work_phone :: Maybe Text,
    format_preference :: Maybe Text,
    social_user_id :: Maybe Text
  }
  deriving (Show, Generic)

instance SqlRow Player

player :: Table Player
player = table "tennis_player" 
  [ #id :- autoPrimary
  , #user_id :- foreignKey user #id
  ]

$(deriveJSON defaultOptions ''Player)

-- Group
data Group = Group
  { id :: ID Group,
    name :: Text
  }
  deriving (Show, Generic)

instance SqlRow Group

group :: Table Group
group = table "auth_group"
  [ #id :- autoPrimary
  ]

$(deriveJSON defaultOptions ''Group)

-- Org
data Org = Org
  { id :: ID Org,
    name :: Text,
    abbrev :: Text,
    administrator_id :: ID User,
    members_group_id :: ID Group
  }
  deriving (Show, Generic)

instance SqlRow Org
org :: Table Org
org = table "tennis_organization"
  [ #id :- autoPrimary
  ]

$(deriveJSON defaultOptions ''Org)

-- League
data League = League
  { id :: ID League,
    name :: Text,
    abbrev :: Maybe Text,
    org_id :: ID Org,
    year :: Int,
    season :: Text,
    division:: Text,
    begin_date :: Day,
    is_knockout :: Bool
  }
  deriving (Show, Generic)

instance SqlRow League
league :: Table League
league = table "tennis_league"
  [ #id :- autoPrimary
  , #org_id :- foreignKey org #id
  ]

$(deriveJSON defaultOptions ''League)

-- Event
data Event = Event
  { id :: ID Event,
    date :: UTCTime,
    name :: Text,
    org_id :: ID Org,
    event_type :: Text,
    comment :: Maybe Text,
    always_show :: Bool,
    league_id :: Maybe (ID League)
  }
  deriving (Show, Generic)

instance SqlRow Event
event :: Table Event
event = table "tennis_event"
  [ #id :- autoPrimary
  , #org_id :- foreignKey org #id
  , #league_id :- foreignKey league #id
  ]

$(deriveJSON defaultOptions ''Event)

-- EventRsvp
data EventRsvp = EventRsvp
  { id :: ID EventRsvp,
    event_id :: ID Event,
    player_id :: ID Player,
    response :: Text,
    comment :: Text
  }
  deriving (Show, Generic)

instance SqlRow EventRsvp
eventrsvp :: Table EventRsvp
eventrsvp = table "tennis_eventrsvp"
  [ #id :- autoPrimary
  , #event_id :- foreignKey event #id
  , #player_id :- foreignKey player #id
  ]

$(deriveJSON defaultOptions ''EventRsvp)

-- Match
data Match = Match
  { id :: ID Match,
    date :: UTCTime,
    league_id :: ID League,
    home_player1_id :: ID Player,
    home_player2_id :: ID Player,
    away_player1_id :: ID Player,
    away_player2_id :: ID Player,
    home_won :: Bool,
    score:: Text,
    comment:: Text,
    round_num::Int,
    match_num::Int
  }
  deriving (Show, Generic)

instance SqlRow Match
match :: Table Match
match = table "tennis_match"
  [ #id :- autoPrimary
  , #league_id :- foreignKey league #id
  , #home_player1_id :- foreignKey player #id
  , #home_player2_id :- foreignKey player #id
  , #away_player1_id :- foreignKey player #id
  , #away_player2_id :- foreignKey player #id
  ]

$(deriveJSON defaultOptions ''Match)

-- PPrefs

data PPrefs = PPrefs
  { id :: ID PPrefs,
    player_id :: ID Player,
    rating_adj :: Double,
    p1_id:: ID Player,
    p2_id:: ID Player, 
    p3_id:: ID Player 
  }
  deriving (Show, Generic)

instance SqlRow PPrefs
pprefs :: Table PPrefs
pprefs = table "tennis_pprefs"
  [ #id :- autoPrimary
  , #player_id :- foreignKey player #id
  ]
$(deriveJSON defaultOptions ''PPrefs)

-- Registration

data Registration = Registration
  { id :: ID Registration,
    league_id :: ID League,
    player_id :: ID Player
  }
  deriving (Show, Generic)

instance SqlRow Registration
registration :: Table Registration
registration = table "tennis_registration"
  [ #id :- autoPrimary
  , #league_id :- foreignKey league #id
  , #player_id :- foreignKey player #id
  ]
$(deriveJSON defaultOptions ''Registration)


-- Facility

data Facility = Facility
  { id :: ID Facility,
    name :: Text,
    abbrev :: Maybe Text,
    phone :: Maybe Text,
    address :: Maybe Text,
    map_url :: Maybe Text
  }
  deriving (Show, Generic)

instance SqlRow Facility
facility :: Table Facility
facility = table "tennis_facility"
  [ #id :- autoPrimary
  ]
$(deriveJSON defaultOptions ''Facility)