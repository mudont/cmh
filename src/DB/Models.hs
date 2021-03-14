module DB.Models where

import           ClassyPrelude
import           Data.Aeson                (defaultOptions)
import           Data.Aeson.TH             (deriveJSON)
-- import Data.Fixed (Pico)
-- import Data.Int (Int32, Int64)
-- import Data.Maybe (fromMaybe, isNothing)
-- import Data.Text (Text, pack)
import           Database.Selda            hiding (Group)
import           Database.Selda.PostgreSQL ()

data SiteType = Cricket | Tennis | Exchange | Other
  deriving (Show, Read, Bounded, Enum)

instance SqlType SiteType

$(deriveJSON defaultOptions ''SiteType)

-- COMMON TABLES
data User = User
  { id         :: ID User,
    username   :: Text,
    password   :: Text,
    email      :: Maybe Text,
    created_at :: UTCTime,
    updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow User

user :: Table User
user = table "user" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''User)

data Site = Site
  { id          :: ID Site,
    name        :: Text,
    owner       :: ID User,
    domain_name :: Maybe Text,
    site_type   :: SiteType,
    created_at  :: UTCTime,
    updated_at  :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Site

$(deriveJSON defaultOptions ''Site)

site :: Table Site
site = table "site" [#id :- primary]

data Person = Person
  { id             :: ID Person,
    user_id        :: Maybe (ID User),
    first_name     :: Text,
    middle_name    :: Maybe Text,
    last_name      :: Text,
    email          :: Maybe Text,
    phone          :: Maybe Text,
    login          :: Text,
    password       :: Text,
    is_staff       :: Bool,
    is_active      :: Bool,
    is_superuser   :: Bool,
    per_created_at :: UTCTime,
    per_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Person

person :: Table Person
person = table "person" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''Person)

data Group = Group
  { id         :: ID Group,
    site_id    :: ID Site,
    name       :: Text,
    created_at :: UTCTime,
    updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Group

$(deriveJSON defaultOptions ''Group)

group :: Table Group
group = table "group" [#id :- autoPrimary]

data GroupMember = GroupMember
  { id         :: ID GroupMember,
    person_id  :: ID Person,
    grp_id     :: ID Group,
    created_at :: UTCTime,
    updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow GroupMember

$(deriveJSON defaultOptions ''GroupMember)

groupMember :: Table GroupMember
groupMember = table "group_member" [#id :- primary, (#person_id :+ #grp_id) :- unique]

{-
check_perm(obj_type, obj_id, org_id, user_id ):
    session.user.is_superuser ||
    session.user == org(org_id).owner||
    (   from p in Db.Permissions
        where p.ObjectType == objectType &&
            ((p.ObjectId == objectId || p.ObjectId == null) &&
            (p.UserId == session.UserAuthId || p.UserId == null) &&
            (session.Groups.Contains(p.GroupId) || p.GroupId == null))
        orderby p.ObjectId descending, p.UserId descending, p.Permitted, p.GroupId descending
        select p.Permitted
    ).FirstOrDefault();
-}
data Permission = Permission
  { id          :: ID Permission,
    site_id     :: ID Site,
    object_type :: Text,
    object_id   :: Maybe Int,
    user_id     :: Maybe (ID User),
    group_id    :: Maybe (ID Group),
    created_at  :: UTCTime,
    updated_at  :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Permission

$(deriveJSON defaultOptions ''Permission)

permission :: Table Permission
permission = table "permission" [#id :- primary, (#object_type :+ #object_id) :- unique]

-- POLLS
data Poll = Poll
  { id             :: ID Poll,
    site_id        :: ID Site,
    date           :: Day,
    name           :: Text,
    per_created_at :: UTCTime,
    per_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Poll

$(deriveJSON defaultOptions ''Poll)

poll :: Table Poll
poll = table "poll" [#id :- primary]

data PollChoice = PollChoice
  { id             :: ID PollChoice,
    poll_id        :: ID Poll,
    name           :: Text,
    per_created_at :: UTCTime,
    per_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow PollChoice

$(deriveJSON defaultOptions ''PollChoice)

pollChoice :: Table PollChoice
pollChoice = table "poll_choice" [#id :- primary]

data PollVote = PollVote
  { id             :: ID PollVote,
    poll_id        :: ID Poll,
    user_id        :: ID User,
    choice         :: Text,
    per_created_at :: UTCTime,
    per_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow PollVote

$(deriveJSON defaultOptions ''PollVote)

pollVote :: Table PollVote
pollVote = table "poll_vote" [#id :- primary]

-- TENNIS TABLES
-- POLL/EVENT TABLES
-- EXCHANGE TABLES
-- CRICKET TABLES
