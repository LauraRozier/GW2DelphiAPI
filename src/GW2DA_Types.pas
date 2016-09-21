unit GW2DA_Types;

interface
uses
  // System units
  SysUtils, Classes;

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
    v1Build, v1Colors, v1Continents, v1EventDetails, v1EventNames, v1Events,
    v1Files, v1GuildDetails, v1ItemDetails, v1Items, v1MapFloor, v1MapNames,
    v1Maps, v1RecipeDetails, v1Recipes, v1SkinDetails, v1Skins, v1WorldNames,
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


  TRGBArray = array [0..2] of Integer;

  TErrorMessage = record
    HadError: Boolean;
    Msg:      string;
  end;

  TUrlParam = record
    Name, Value: string;
  end;

  TUrlParams = array of TUrlParam;

  TGW2Version = class
    private
      fId: Integer;
    public
      Property id: Integer Read fId Write fId;
  end;

  TGW2Quaggan = class
    private
      fId:  string;
      fUrl: string;
    public
      property id:  string Read fId  Write fId;
      property url: string Read fUrl Write fUrl;
  end;

  TGW2World = class
    private
      fId:   Integer;
      fName: string;
    public
      property id:   Integer Read fId   Write fId;
      property Name: string  Read fName Write fName;
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
      fId:      Integer;
      fName:    string;
      fBaseRGB: TRGBArray;
      fCloth:   TGW2ColorDetail;
      fLeather: TGW2ColorDetail;
      fMetal:   TGW2ColorDetail;
      Function GetRGB(aIndex: Integer): Integer;
      Procedure SetRGB(aIndex, aValue: Integer);
    public
      property id:                       Integer         Read fId      Write fId;
      property Name:                     string          Read fName    Write fName;
      property BaseRGB[aIndex: Integer]: Integer         Read GetRGB   Write SetRGB;
      property Cloth:                    TGW2ColorDetail Read fCloth   Write fCloth;
      property Leather:                  TGW2ColorDetail Read fLeather Write fLeather;
      property Metal:                    TGW2ColorDetail Read fMetal   Write fMetal;
  end;

implementation 

{ TGW2ColorDetail }
Function TGW2ColorDetail.GetRGB(aIndex: Integer): Integer;
begin
  Result := fRGB[aIndex];
end;


Procedure TGW2ColorDetail.SetRGB(aIndex, aValue: Integer);
begin
  fRGB[aIndex] := aValue;
end;

 
{ TGW2Color }
Function TGW2Color.GetRGB(aIndex: Integer): Integer;
begin
  Result := fBaseRGB[aIndex];
end;


Procedure TGW2Color.SetRGB(aIndex, aValue: Integer);
begin
  fBaseRGB[aIndex] := aValue;
end;

end.
