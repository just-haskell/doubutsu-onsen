
CREATE SCHEMA DOUBUTSU;

-- user が SQL の keyword らしいので避けた

-- * ユーザー
CREATE TABLE DOUBUTSU.gameuser
  ( seq BIGINT NOT NULL
  , email VARCHAR(256) NOT NULL
  , username text NOT NULL
  , create_time TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (seq)
  );

-- * イベント
CREATE TABLE DOUBUTSU.event
  ( seq BIGINT NOT NULL
  , uid BIGINT NOT NULL
  , onsen_id INTEGER NOT NULL
  , typ INTEGER NOT NULL
  , event_data INTEGER NOT NULL
  , create_time TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (seq)
  );

-- ** 温泉のはじまり
--  typ == 0
--  event_data == <温泉ステージID>

-- ** 動物の行動
--  typ == 1
CREATE TABLE DOUBUTSU.event_action
  ( seq INTEGER NOT NULL
  , doubutsu_id INTEGER NOT NULL
  , action_type INTEGER NOT NULL -- 来る: 0 , 帰る: 1 , or 癒しポーズID

  , PRIMARY KEY (seq)
  );

-- ** 種や木の実をもらう - 属性
--  typ == 2
CREATE TABLE DOUBUTSU.event_item
  ( seq INTEGER NOT NULL
  , item_type INTEGER NOT NULL
  , reason_id INTEGER NOT NULL

  , PRIMARY KEY (seq)
  );

-- ** 種や木の実を植える
--  typ == 3
--  event_data == <種や木の実の種類のID>

-- ** 種や木の実の成長
--  typ == 4
CREATE TABLE DOUBUTSU.event_growth
  ( seq INTEGER NOT NULL
  , item_type INTEGER NOT NULL
  , growth_level INTEGER NOT NULL

  , PRIMARY KEY (seq)
  );

-- ** ミッション達成
--  typ == 5
--  event_data == <温泉のレベル>
--  onsen_id とレベルにより特定ミッションが対応付けられる

-- ** 時間が経つ
--  typ == 5
--  event_data == <朝: 0 , 昼: 1 , 夕方: 2 , 夜: 3 のどれか>

-- ** 天気が変わる
--  typ == 6
--  event_data == <天気の状態のID>
