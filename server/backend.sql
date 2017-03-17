
CREATE SCHEMA DOUBUTSU;

-- Rails っぽい命名規則を採用
-- 整数値 primary key -- id
-- foreign key 参照 -- xxx_id
-- 時刻 -- xxx_at -- e.g. updated_at, created_at
-- 日付 -- xxx_on

-- foreign key 制約を利用するのは backup を考えたときの問題が無いと確認できてからにする

-- * ユーザー
CREATE TABLE DOUBUTSU.gameuser -- user が SQL の keyword らしいので避けた
  ( id BIGINT NOT NULL
  , email VARCHAR(256) NOT NULL
  , username TEXT NOT NULL
  , created_at TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (id)
  );


-- * イベント
CREATE TABLE DOUBUTSU.event
  ( id BIGINT NOT NULL
  , gameuser_id BIGINT NOT NULL
  , onsen_id INTEGER NOT NULL
  , typ INTEGER NOT NULL
  , event_data INTEGER NOT NULL
  , created_at TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (id)
  );

-- ** 温泉のはじまり
--  typ == 0
--  event_data == <温泉ステージID>

-- ** 動物の行動
--  typ == 1
CREATE TABLE DOUBUTSU.event_action
  ( id INTEGER NOT NULL
  , slot_id INTEGER NOT NULL
  , doubutsu_id INTEGER NOT NULL
  , action_type INTEGER NOT NULL -- 来る: 0 , 帰る: 1 , or 癒しポーズID

  , PRIMARY KEY (id)
  );

-- ** 種や木の実をもらう - 属性
--  typ == 2
CREATE TABLE DOUBUTSU.event_item
  ( id INTEGER NOT NULL
  , item_id INTEGER NOT NULL
  , got_reason_type INTEGER NOT NULL
  , got_reason_data INTEGER NOT NULL

  , PRIMARY KEY (id)
  );

-- ** 種や木の実の成長
--  typ == 3
CREATE TABLE DOUBUTSU.event_growth
  ( id INTEGER NOT NULL
  , slot_id INTEGER NOT NULL
  , item_id INTEGER NOT NULL
  , growth_level INTEGER NOT NULL -- 植えたときはレベル1

  , PRIMARY KEY (id)
  );

-- ** ミッション達成
--  typ == 4
--  event_data == <温泉のレベル>
--  onsen_id とレベルにより特定ミッションが対応付けられる

-- ** 時間が経つ
--  typ == 5
--  event_data == <朝: 0 , 昼: 1 , 夕方: 2 , 夜: 3 のどれか>

-- ** 天気が変わる
--  typ == 6
--  event_data == <天気の状態のID>


-- * 種や木の実の種類 - 属性
CREATE TABLE DOUBUTSU.item
  ( id INTEGER NOT NULL
  , item_name TEXT NOT NULL
  , rarity INTEGER NOT NULL  -- 通常:0 , レア:1 , 激レア:2
  )


-- * 種や木の実をもらった理由詳細

-- ** 動物が帰ったときにもらう - 属性
-- got_reason_type == 0
-- got_reason_data == <動物のID>

-- ** その他
-- got_reason_type == 1
-- got_reason_data == <イベントのID?>
