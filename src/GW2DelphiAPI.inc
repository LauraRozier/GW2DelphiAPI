uses
  // System units
  SysUtils, Classes, RegularExpressions, REST.JSON, JSON, TypInfo, SysConst,
  // Indy units
  IdHTTP, IdException, IdExceptionCore, IdStack, IdSSLOpenSSL;

{ API Enum types }
type
  TAPIVersion = (
    APINone, APIv1, APIv2
  );

  TAPILangs = (
    // API v1 and v2
    langEN, langES, langDE, langFR,
    // API v2 only
    langKO, langZH
  );

  TAPIFunction = (
    // API version 1
    v1Continents, v1EventDetails, v1EventNames, v1Events, v1Files,
    v1GuildDetails, v1ItemDetails, v1Items, v1MapFloor, v1MapNames, v1Maps,
    v1RecipeDetails, v1Recipes, v1SkinDetails, v1Skins, v1WorldNames,
    v1WvwMatchDetails, v1WvwMatches, v1WvwObjectiveNames,
    // API version 2
    v2Account, v2AccountAchievements, v2AccountBank, v2AccountDyes,
    v2AccountFinishers, v2AccountInventory, v2AccountMasteries,
    v2AccountMaterials, v2AccountMinis, v2AccountOutfits, v2AccountRecipes,
    v2AccountSkins, v2AccountTitles, v2AccountWallet, v2Achievements,
    v2AchievementsCategories, v2AchievementsDaily, v2AchievementsDailyTomorrow,
    v2AchievementsGroups, v2Adventures, v2AdventuresLeaderboards,
    v2AdventuresLeaderboardsBoard, v2BackstoryAnswers, v2BackstoryQuestions,
    v2Build, v2Characters, v2CharactersBackstory, v2CharactersCore,
    v2CharactersCrafting, v2CharactersEquipment, v2CharactersHeropoints,
    v2CharactersInventory, v2CharactersRecipes, v2CharactersSpecializations,
    v2CharactersTraining, v2Colors, v2CommerceExchange, v2CommerceListings,
    v2CommercePrices, v2CommerceTransactions, v2Continents, v2Currencies,
    v2Emblem, v2Events, v2EventsState, v2Files, v2Finishers, v2GuildID,
    v2GuildIDLog, v2GuildIDMembers, v2GuildIDRanks, v2GuildIDStash,
    v2GuildIDTeams, v2GuildIDTreasury, v2GuildIDUpgrades, v2GuildPermissions,
    v2GuildSearch, v2GuildUpgrades, v2Items, v2Itemstats, v2Legends, v2Maps,
    v2Masteries, v2Materials, v2Minis, v2Outfits, v2Pets, v2Professions, v2Pvp,
    v2PvpAmulets, v2PvpGames, v2PvpSeasons, v2PvpSeasonsLeaderboards,
    v2PvpSeasonsLeaderboardsBoard, v2PvpStandings, v2PvpStats, v2Quaggans,
    v2Recipes, v2RecipesSearch, v2Skills, v2Skins, v2Specializations, v2Stories,
    v2StoriesSeasons, v2Titles, v2Tokeninfo, v2Traits, v2Worlds, v2WvwAbilities,
    v2WvwMatches, v2WvwObjectives
  );

  TAPIPermissions = (
    None,
    PermAccount,
    PermCharacter,
    PermInventory,
    PermTradingpost,
    PermWallet,
    PermUnlocks,
    PermPvP,
    PermBuilds,
    PermProgression,
    PermGuilds,
    PermSize
  );

{ API data types }
  // Arrays
  TIntegerArray = array of Integer;
  TStringArray  = array of string;
  TRGBArray     = array [0..2] of Integer;

  // Errors
  TErrorMessage = record
    HadError: Boolean;
    Msg:      string;
  end;

  // Parameters
  TUrlParam = record
    Name, Value: string;
  end;

  TUrlParams = array of TUrlParam;

  // Authentication
  TGW2Token = class
    private
      fId:          string;
      fName:        string;
	    fPermissions: TStringArray;
    public
      property Id:          string       Read fId          Write fId;
      property Name:        string       Read fName        Write fName;
      property Permissions: TStringArray Read fPermissions Write fPermissions;
  end;

  // Misc
  TGW2Version = class
    private
      fId: Integer;
    public
      Property id: Integer Read fId Write fId;
  end;

  TGW2ColorDetail = class
    private
      fBrightness: Integer;
      fContrast:   Double;
      fHue:        Integer;
      fSaturation: Double;
      fLightness:  Double;
      fRGB:        TRGBArray;
      Function GetRGB(aIndex: Integer): Integer;
      Procedure SetRGB(aIndex, aValue: Integer);
    public
      property Brightness:           Integer Read fBrightness Write fBrightness;
      property Contrast:             Double  Read fContrast   Write fContrast;
      property Hue:                  Integer Read fHue        Write fHue;
      property Saturation:           Double  Read fSaturation Write fSaturation;
      property Lightness:            Double  Read fLightness  Write fLightness;
      property RGB[aIndex: Integer]: Integer Read GetRGB      Write SetRGB;
  end;

  TGW2Color = class
    private
      fId:         Integer;
      fName:       string;
      fBase_RGB:   TRGBArray;
      fCloth:      TGW2ColorDetail;
      fLeather:    TGW2ColorDetail;
      fMetal:      TGW2ColorDetail;
      fItem:       Integer;
      fCategories: TStringArray;
      Function GetRGB(aIndex: Integer): Integer;
      Procedure SetRGB(aIndex, aValue: Integer);
    public
      property id:                        Integer         Read fId         Write fId;
      property Name:                      string          Read fName       Write fName;
      property Base_RGB[aIndex: Integer]: Integer         Read GetRGB      Write SetRGB;
      property Cloth:                     TGW2ColorDetail Read fCloth      Write fCloth;
      property Leather:                   TGW2ColorDetail Read fLeather    Write fLeather;
      property Metal:                     TGW2ColorDetail Read fMetal      Write fMetal;
      property Item:                      Integer         Read fItem       Write fItem;
      property Categories:                TStringArray    Read fCategories Write fCategories;
  end;

  TGW2ColorArray = array of TGW2Color;

  TGW2Quaggan = class
    private
      fId:  string;
      fUrl: string;
    public
      property id:  string Read fId  Write fId;
      property url: string Read fUrl Write fUrl;
  end;

  TGW2QuagganArray = array of TGW2Quaggan;

  TGW2World = class
    private
      fId:         Integer;
      fName:       string;
      fPopulation: string;
    public
      property id:         Integer Read fId         Write fId;
      property Name:       string  Read fName       Write fName;
      property Population: string  Read fPopulation Write fPopulation;
  end;

  TGW2WorldArray = array of TGW2World;

  TGW2Currency = class
    private
      fId:          Integer;
      fName:        string;
      fDescription: string;
      fIcon:        string;
      fOrder:       Integer;
    public
      property id:          Integer Read fId          Write fId;
      property Name:        string  Read fName        Write fName;
      property Description: string  Read fDescription Write fDescription;
      property Icon:        string  Read fIcon        Write fIcon;
      property Order:       Integer Read fOrder       Write fOrder;
  end;

  TGW2CurrencyArray = array of TGW2Currency;

  TGW2File = class
    private
      fId:   string;
      fIcon: string;
    public
      property id:   string Read fId   Write fId;
      property Icon: string Read fIcon Write fIcon;
  end;

  TGW2FileArray = array of TGW2File;

  TGW2Mini = class
    private
      fId:      Integer;
      fName:    string;
      fUnlock:  string;
      fIcon:    string;
      fOrder:   Integer;
      fItem_ID: Integer;
    public
      property id:      Integer Read fId      Write fId;
      property Name:    string  Read fName    Write fName;
      property Unlock:  string  Read fUnlock  Write fUnlock;
      property Icon:    string  Read fIcon    Write fIcon;
      property Order:   Integer Read fOrder   Write fOrder;
      property Item_ID: Integer Read fItem_ID Write fItem_ID;
  end;

  TGW2MiniArray = array of TGW2Mini;

  TGW2Account = class
    private
      fId:            string;
      fName:          string;
      fWorld:         Integer;
      fCommander:     Boolean;
      fGuilds:        TStringArray;
      fCreated:       string;
      fAccess:        string;
      fFractal_level: Integer;
      fDaily_ap:      Integer;
      fMonthly_ap:    Integer;
      fWvw_rank:      Integer;
    public
      property id:            string       Read fId            Write fId;
      property Name:          string       Read fName          Write fName;
      property World:         Integer      Read fWorld         Write fWorld;
      property Commander:     Boolean      Read fCommander     Write fCommander;
      property Guilds:        TStringArray Read fGuilds        Write fGuilds;
      property Created:       string       Read fCreated       Write fCreated; // ISO-8601 standard timestamp
      property Access:        string       Read fAccess        Write fAccess;
      {
        Possible values for Access:
          'None'          – should probably never happen
          'PlayForFree'   – has not yet purchased the game
          'GuildWars2'    – has purchased base game, but no expansions
          'HeartOfThorns' – has purchased HoT
      }
      // The properties below require the progression permission flag
      property Fractal_level: Integer      Read fFractal_level Write fFractal_level;
      property Daily_ap:      Integer      Read fDaily_ap      Write fDaily_ap;
      property Monthly_ap:    Integer      Read fMonthly_ap    Write fMonthly_ap;
      property Wvw_rank:      Integer      Read fWvw_rank      Write fWvw_rank;
  end;

  TGW2AccountAchievement = class
    private
      fId:       Integer;
      fDone:     Boolean;
      fUnlocked: Boolean;
      fBits:     TIntegerArray;
      fCurrent:  Integer;
      fMax:      Integer;
      fRepeated: Integer;
    public
      property id:       Integer       Read fId       Write fId;
      property Done:     Boolean       Read fDone     Write fDone;
      property Unlocked: Boolean       Read fUnlocked Write fUnlocked;
      property Bits:     TIntegerArray Read fBits     Write fBits;
      property Current:  Integer       Read fCurrent  Write fCurrent;
      property Max:      Integer       Read fMax      Write fMax;
      property Repeated: Integer       Read fRepeated Write fRepeated;
  end;

  TGW2AccountAchievementArray = array of TGW2AccountAchievement;

  TGW2AccountBankItem = class
    private
      fId:        Integer;
      fCount:     Integer;
      fBinding:   string;
      fSkin:      Integer;
      fUpgrades:  TIntegerArray;
      fInfusions: TIntegerArray;
      fBoundTo:   string;
    public
      property id:        Integer       Read fId        Write fId;
      property Count:     Integer       Read fCount     Write fCount;
      property Binding:   string        Read fBinding   Write fBinding;
      property Skin:      Integer       Read fSkin      Write fSkin;
      property Upgrades:  TIntegerArray Read fUpgrades  Write fUpgrades;
      property Infusions: TIntegerArray Read fInfusions Write fInfusions;
      property Bound_to:  string        Read fBoundTo   Write fBoundTo;
  end;

  TGW2AccountBankItemArray = array of TGW2AccountBankItem;

  // States
  TStateHoler = record
    HTTPTimeout: Integer;
    HTTPClient:  TIdHTTP;
    AuthString:  string;
    AuthToken:   TGW2Token;
  end;

{ Default values }
const
  // Generic constants
  CONST_ONE_SECOND = 1000;

  // GW2 Delphi API constants
  CONST_VERSION_PREFIX   = 'Alpha ';
  CONST_VERSION_MAJOR    = '0';
  CONST_VERSION_MINOR    = '.001';
  CONST_VERSION_BUILD    = '.0001 ';
  CONST_VERSION_REVISION = 'r0042';
  CONST_VERSION          = CONST_VERSION_PREFIX + CONST_VERSION_MAJOR +
                           CONST_VERSION_MINOR  + CONST_VERSION_BUILD +
                           CONST_VERSION_REVISION;
  CONST_VERSION_SHORT    = CONST_VERSION_MAJOR + CONST_VERSION_MINOR;

  // GW2 API constants
  CONST_API_URL_BASE = 'https://api.guildwars2.com/';

  CONST_API_Versions: array [TAPIVersion] of string = (
    '', 'v1', 'v2'
  );

  CONST_API_Languages: array [TAPILangs] of string = (
    // API v1 and v2
    'en', 'es', 'de', 'fr',
    // API v2 only
    'ko', 'zh'
  );

  CONST_API_Functions: array [TAPIFunction] of string = (
    // API version 1
    'continents.json', 'event_details.json', 'event_names.json', 'events.json',
    'files.json', 'guild_details.json', 'item_details.json', 'items.json',
    'map_floor.json', 'map_names.json', 'maps.json', 'recipe_details.json',
    'recipes.json', 'skin_details.json', 'skins.json', 'world_names.json',
    'wvw/match_details.json', 'wvw/matches.json', 'wvw/objective_names.json',
    // API version 2
    'account', 'account/achievements', 'account/bank', 'account/dyes',
    'account/finishers', 'account/inventory', 'account/masteries',
    'account/materials', 'account/minis', 'account/outfits', 'account/recipes',
    'account/skins', 'account/titles', 'account/wallet', 'achievements',
    'achievements/categories', 'achievements/daily',
    'achievements/daily/tomorrow', 'achievements/groups', 'adventures',
    'adventures/%s/leaderboards', 'adventures/%s/leaderboards/%s',
    'backstory/answers', 'backstory/questions', 'build', 'characters',
    'characters/%s/backstory', 'characters/%s/core', 'characters/%s/crafting',
    'characters/%s/equipment', 'characters/%s/heropoints',
    'characters/%s/inventory', 'characters/%s/recipes',
    'characters/%s/specializations', 'characters/%s/training', 'colors',
    'commerce/exchange', 'commerce/listings', 'commerce/prices',
    'commerce/transactions', 'continents', 'currencies', 'emblem', 'events',
    'events-state', 'files', 'finishers', 'guild/%s', 'guild/%s/log',
    'guild/%s/members', 'guild/%s/ranks', 'guild/%s/stash', 'guild/%s/teams',
    'guild/%s/treasury', 'guild/%s/upgrades', 'guild/permissions',
    'guild/search', 'guild/upgrades', 'items', 'itemstats', 'legends', 'maps',
    'masteries', 'materials', 'minis', 'outfits', 'pets', 'professions', 'pvp',
    'pvp/amulets', 'pvp/games', 'pvp/seasons', 'pvp/seasons/%s/leaderboards',
    'pvp/seasons/%s/leaderboards/%s', 'pvp/standings', 'pvp/stats', 'quaggans',
    'recipes', 'recipes/search', 'skills', 'skins', 'specializations',
    'stories', 'stories/seasons', 'titles', 'tokeninfo', 'traits', 'worlds',
    'wvw/abilities', 'wvw/matches', 'wvw/objectives'
  );

{ Web handler class }
type
  TWebHandler = class(TObject)
    private
      fHTTPClient: TIdHTTP;
      function SendRequest(aUrl: string): TErrorMessage;
      function BuildParamString(aParams: TUrlParams): string;
    public
      constructor Create;
      function FetchRawEndpoint(aUrl: string): string; Overload;
      function FetchRawEndpoint<T: class, constructor>(aUrl: string): T; Overload;
      function FetchEndpoint(aVersion: TAPIVersion; aFunction: TAPIFunction;
                             aParams: TUrlParams): string; Overload;
      function FetchEndpoint<T: class, constructor>(aVersion: TAPIVersion; aFunction: TAPIFunction;
                                aParams: TUrlParams): T; Overload;
      function FetchAuthEndpoint(aVersion: TAPIVersion; aFunction: TAPIFunction;
                                 aParams: TUrlParams;
                                 aAuthString: string): string; Overload;
      function FetchAuthEndpoint<T: class, constructor>(aVersion: TAPIVersion; aFunction: TAPIFunction;
                                    aParams: TUrlParams;
                                    aAuthString: string): T; Overload;
      Property HTTPClient: TIdHTTP Read fHTTPClient Write fHTTPClient;
  end;

{ Utilities }
  TGW2Helper = class
    public
      function StringToEnum<TEnum>(const aString: string): TEnum;
      function EnumToInt<TEnum>(const aEnumValue: TEnum): Integer;
      function EnumToString<TEnum>(const aEnumValue: TEnum): string;
      function GetTokenInfo(aWebHandler: TWebHandler; aAuthStr: string): TGW2Token;
      function ArrContains(aArr: TStringArray; aValue: string): Boolean; Overload;
      function ArrContains(aArr: TIntegerArray; aValue: Integer): Boolean; Overload;
  end;

type
{ API Misc functions class }
  TGW2APIMisc = class(TObject)
    public
      function GetBuild(aWebHandler:       TWebHandler):                      TGW2Version;
      function GetColorIDs(aWebHandler:    TWebHandler):                      TIntegerArray;
      function GetColors(aWebHandler:      TWebHandler; aParams: TUrlParams): TGW2ColorArray;
      function GetQuagganIDs(aWebHandler:  TWebHandler):                      TStringArray;
      function GetQuaggans(aWebHandler:    TWebHandler; aParams: TUrlParams): TGW2QuagganArray;
      function GetWorldIDs(aWebHandler:    TWebHandler):                      TIntegerArray;
      function GetWorlds(aWebHandler:      TWebHandler; aParams: TUrlParams): TGW2WorldArray;
      function GetCurrencyIDs(aWebHandler: TWebHandler):                      TIntegerArray;
      function GetCurrencies(aWebHandler:  TWebHandler; aParams: TUrlParams): TGW2CurrencyArray;
      function GetFileIDs(aWebHandler:     TWebHandler):                      TStringArray;
      function GetFiles(aWebHandler:       TWebHandler; aParams: TUrlParams): TGW2FileArray;
      function GetMiniIDs(aWebHandler:     TWebHandler):                      TIntegerArray;
      function GetMinis(aWebHandler:       TWebHandler; aParams: TUrlParams): TGW2MiniArray;
  end;

TGW2APIAccount = class(TObject)
  public
    function GetAccount(aWebHandler: TWebHandler; aState: TStateHoler): TGW2Account;
    function GetAchievements(aWebHandler: TWebHandler; aState: TStateHoler): TGW2AccountAchievementArray;
    function GetBank(aWebHandler: TWebHandler; aState: TStateHoler): TGW2AccountBankItemArray;
end;

{ Main API class }
  TGW2API = class(TObject)
    private
      fStateHolder: TStateHoler;
      fWebHandler:  TWebHandler;
      fUtils:       TGW2Helper;
      fMisc:        TGW2APIMisc;
      fAccount:     TGW2APIAccount;
    Public
      constructor Create(aTimeoutSeconds: Integer = 15);
      destructor Destroy; Override;
      procedure SetTimeout(aSeconds: SmallInt);
      function Authenticate(aAuthString: string): string;
      Property State:      TStateHoler    Read fStateHolder;
      Property WebHandler: TWebHandler    Read fWebHandler;
      Property Utils:      TGW2Helper     Read fUtils;
      Property Misc:       TGW2APIMisc    Read fMisc;
      Property Account:    TGW2APIAccount Read fAccount;
  end;
