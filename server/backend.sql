
CREATE SCHEMA DOUBUTSU;

-- Rails っぽい命名規則を採用
-- 整数値 primary key -- id
-- foreign key 参照 -- xxx_id
-- 時刻 -- xxx_at -- e.g. updated_at, created_at
-- 日付 -- xxx_on

-- foreign key 制約を利用するのは backup を考えたときの問題が無いと確認できてからにする

-- * ゲーム
CREATE TABLE DOUBUTSU.game
  ( id BIGINT NOT NULL
  , gameuser_id BIGINT NOT NULL

  , PRIMARY KEY (id)
  );

-- * ユーザー
CREATE TABLE DOUBUTSU.gameuser -- user が SQL の keyword らしいので避けた
  ( id BIGINT NOT NULL
  , game_id BIGINT NOT NULL
  , email VARCHAR(256) NOT NULL
  , username TEXT NOT NULL
  , created_at TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (id)
  );

-- * 温泉 (ステージ?) - 属性
CREATE TABLE DOUBUTSU.onsen
  ( id INTEGER NOT NULL
  , onsen_name TEXT NOT NULL
  , released_at TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (id)
  );

-- * スロット (動物が温泉に入ったり、カスタマイズ用の木の種や実を植えたりする) - 属性
CREATE TABLE DOUBUTSU.slot
  ( id INTEGER NOT NULL
  , onsen_id INTEGER NOT NULL
  , local_slot_number INTEGER NOT NULL
  , locate_x INTEGER NOT NULL
  , locate_y INTEGER NOT NULL
  , slot_type INTEGER NOT NULL  -- 動物が温泉に入る: 0, カスタマイズ: 1

  , PRIMARY KEY (id)
  );

CREATE UNIQUE INDEX slot_onsen_slotnum ON DOUBUTSU.slot (onsen_id, local_slot_number);

-- * 種や木の実の種類 - 属性
CREATE TABLE DOUBUTSU.item
  ( id INTEGER NOT NULL
  , item_name TEXT NOT NULL
  , rarity INTEGER NOT NULL  -- 通常:0 , レア:1 , 激レア:2

  , PRIMARY KEY (id)
  );

-- * 動物の種類(個体) - 属性
CREATE TABLE DOUBUTSU.doubutsu
  ( id INTEGER NOT NULL
  , doubutsu_name TEXT NOT NULL
  , released_at TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (id)
  );

-- * 動物のスロット占有相対座標
CREATE TABLE DOUBUTSU.doubutsu_size
  ( doubutsu_id INTEGER NOT NULL
  , local_coord_number INTEGER NOT NULL
  , relative_x INTEGER NOT NULL
  , relative_y INTEGER NOT NULL

  , PRIMARY KEY (doubutsu_id, local_coord_number)
  );

-- * 種や木の実をもらった理由詳細

-- ** 動物が帰ったときにもらう - 属性
-- got_reason_type == 0
-- got_reason_data == <動物のID>

-- ** その他

-- * 各ミッション詳細
-- ** はじめてのお客さん - 属性
-- mission_type == 0

-- ** 特定の動物がn回、来た - 属性
CREATE TABLE DOUBUTSU.mission_visited
  ( id INTEGER NOT NULL
  , doubutsu_id INTEGER NOT NULL
  , times INTEGER NOT NULL

  , PRIMARY KEY (id)
  );

-- ** 特定の種あるいは木の実をn回、植えた - 属性
-- mission_type == 1
CREATE TABLE DOUBUTSU.mission_used_item
  ( id INTEGER NOT NULL
  , item_id INTEGER NOT NULL
  , times INTEGER NOT NULL

  , PRIMARY KEY (id)
  );

-- ** どうぶつのお悩み - 属性
-- mission_type == 2

-- * ミッション (温泉レベルアップ条件) - 属性
CREATE TABLE DOUBUTSU.mission
  ( onsen_id INTEGER NOT NULL
  , onsen_level INTEGER NOT NULL
  , mission_type INTEGER NOT NULL
  , mission_data INTEGER NOT NULL

  , PRIMARY KEY (onsen_id, onsen_level)
  );


-- * 各イベント詳細
-- ** 温泉のはじまり
--  event_type == 0
--  event_data    未使用: -1 固定

-- ** 動物の行動
--  event_type == 1
CREATE TABLE DOUBUTSU.event_action
  ( id INTEGER NOT NULL
  , slot_id INTEGER NOT NULL
  , doubutsu_id INTEGER NOT NULL
  , action_type INTEGER NOT NULL -- 来る: 0 , 帰る: 1 , or 癒しポーズID

  , PRIMARY KEY (id)
  );

-- ** 種や木の実をもらう - 属性
--  event_type == 2
CREATE TABLE DOUBUTSU.event_item
  ( id INTEGER NOT NULL
  , item_id INTEGER NOT NULL
  , got_reason_type INTEGER NOT NULL
  , got_reason_data INTEGER NOT NULL

  , PRIMARY KEY (id)
  );

-- ** 種や木の実の成長
--  event_type == 3
CREATE TABLE DOUBUTSU.event_growth
  ( id INTEGER NOT NULL
  , slot_id INTEGER NOT NULL
  , item_id INTEGER NOT NULL
  , growth_level INTEGER NOT NULL -- 植えたときはレベル1

  , PRIMARY KEY (id)
  );

-- ** ミッション達成
--  event_type == 4
--  event_data == <温泉のレベル>
--  onsen_id とレベルにより特定ミッションが対応付けられる

-- ** 時間が経つ
--  event_type == 5
--  event_data == <朝: 0 , 昼: 1 , 夕方: 2 , 夜: 3 のどれか>

-- ** 天気が変わる
--  event_type == 6
--  event_data == <天気の状態のID>

-- * イベントログ
CREATE TABLE DOUBUTSU.event_log
  ( id BIGINT NOT NULL
  , gameuser_id BIGINT NOT NULL
  , onsen_id INTEGER NOT NULL
  , event_type INTEGER NOT NULL
  , event_data INTEGER NOT NULL
  , created_at TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (id)
  );


-- * 温泉の状態 - 属性
CREATE TABLE DOUBUTSU.onsen_status
  ( gameuser_id INTEGER NOT NULL
  , onsen_id INTEGER NOT NULL
  , updated_at TIMESTAMPTZ NOT NULL
  , onsen_level INTEGER NOT NULL
  , seed INTEGER NOT NULL
  , started_at TIMESTAMPTZ NOT NULL

  , PRIMARY KEY (gameuser_id, onsen_id)
  );

-- * スロットの状態詳細
-- ** 動物が入る - 属性
-- slot_type == 0
CREATE TABLE DOUBUTSU.slot_status_doubutsu
  ( id INTEGER NOT NULL
  , doubutsu_id INTEGER NOT NULL -- 入っていない: -1
  -- , doubutsu_pause INTEGER NOT NULL

  , PRIMARY KEY (id)
  );

-- ** カスタマイズ - 属性
-- slot_type == 1
CREATE TABLE DOUBUTSU.slot_status_custom
  ( id INTEGER NOT NULL
  , item_id INTEGER NOT NULL -- 何も植えられていない: -1
  , growth_level INTEGER NOT NULL

  , PRIMARY KEY (id)
  );

-- * スロットの状態 - 属性
CREATE TABLE DOUBUTSU.slot_status
  ( gameuser_id INTEGER NOT NULL
  , slot_id INTEGER NOT NULL
  , slot_data INTEGER NOT NULL

  , PRIMARY KEY (gameuser_id, slot_id)
  );
