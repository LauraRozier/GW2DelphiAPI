unit GW2DA_Defaults;

interface
uses
  // System units
  SysUtils, Classes,
  // GW2 Delphi API Units
  GW2DA_Types;

const
  // Generic constants
  CONST_ONE_SECOND = 1000;

  // GW2 Delphi API constants
  CONST_VERSION_PREFIX   = 'Alpha ';
  CONST_VERSION_MAJOR    = '0';
  CONST_VERSION_MINOR    = '.001';
  CONST_VERSION_BUILD    = '.0001 ';
  CONST_VERSION_REVISION = 'r0012';
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
    'build.json', 'colors.json', 'continents.json', 'event_details.json',
    'event_names.json', 'events.json', 'files.json', 'guild_details.json',
    'item_details.json', 'items.json', 'map_floor.json', 'map_names.json',
    'maps.json', 'recipe_details.json', 'recipes.json', 'skin_details.json',
    'skins.json', 'world_names.json', 'wvw/match_details.json',
    'wvw/matches.json', 'wvw/objective_names.json',
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

implementation
  {
    No need for any implementations.
    This file only holds constants.
  }
end.
