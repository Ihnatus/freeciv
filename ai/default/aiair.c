/***********************************************************************
 Freeciv - Copyright (C) 2002 - The Freeciv Team
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
***********************************************************************/

#ifdef HAVE_CONFIG_H
#include <fc_config.h>
#endif

/* utility */
#include "log.h"

/* common */
#include "combat.h"
#include "game.h"
#include "map.h"
#include "movement.h"
#include "player.h"
#include "pf_tools.h"
#include "unit.h"

/* server */
#include "citytools.h"
#include "maphand.h"
#include "srv_log.h"
#include "unithand.h"
#include "unittools.h"

/* server/advisors */
#include "advbuilding.h"
#include "advgoto.h"

/* ai */
#include "handicaps.h"

/* ai/default */
#include "aicity.h"
#include "aiplayer.h"
#include "ailog.h"
#include "aitools.h"
#include "aiunit.h"

#include "aiair.h"

static int find_something_to_bomb(struct ai_type *ait, struct unit *punit,
                                  struct pf_path **path, struct tile **pptile,
                                  bool banzai);

/*****************************************************************//******
  Do an attack by fueled punit if we have a worthy target this turn.
  banzai means the unit itself is considered already lost.
  Returns FALSE if the unit is alive and can move.
*************************************************************************/
static bool dai_do_air_attack(struct ai_type *ait, struct unit *punit,
                              bool banzai)
{
  struct tile *dst_tile = unit_tile(punit);
  struct pf_path *path;

  if (find_something_to_bomb(ait, punit, &path, &dst_tile, banzai) > 0) {
    /* Found target, coordinates are in punit's goto_dest.
     * TODO: check for the best tile to attack from */
    struct unit_type *utype = unit_type_get(punit);
    bool takemeback = FALSE;
    struct unit *ptrans;

    fc_assert_ret_val(path != NULL && dst_tile != NULL, TRUE);
    /* If we happen to carry a bombarder, it should be unloaded
     * to bombard something (but we not always can or want) */
    if (utype_has_flag(utype, UTYF_BOMBARDER)
        && is_tiles_adjacent(dst_tile, unit_tile(punit))
        && !is_ocean_tile(dst_tile)) {
      struct tile *utile = unit_tile(punit);
      ptrans = unit_transport_get(punit);
      if (ptrans && can_unit_unload(punit, ptrans)) {
        struct unit_type *ptrans_type = unit_type_get(ptrans);
        bool hereisgood = can_unit_survive_at_tile(punit, utile);
        /* If we can't freely unload AND get back refueling,
         * we won't do the probably stupid thing at all, chat gets warning
         * FIXME: bombarders are stupid in this file in general */

        if (tile_city(utile) || tile_has_native_base(utile, ptrans_type)
            || utype_can_freely_load(utype, ptrans_type)
            || hereisgood) {
          takemeback = !hereisgood;
          unit_transport_unload_send(punit);
          /* if there is ever unload script event, check existence here */
        }
      }
    }
    if (!adv_follow_path(punit, path, dst_tile)) {
      pf_path_destroy(path);
      return TRUE; /* The unit died. */
    } else if (takemeback) {
      /* if there is ever script event for bombardment,
       * replace the variable check with a check that we actually have
       * reasons and means to load */
      unit_transport_load_send(punit, ptrans);
      /* if there is ever load script event, check existence here */
    }
    pf_path_destroy(path);
    if (punit->moves_left <= 0) {
      return TRUE;
    }
  }
  return FALSE;
}

/**************************************************************************
  Looks for nearest airbase for punit reachable (immediately)
  (if it is on start with moves, if start tile is supplied)
  Returns NULL if not found.  The path is stored in the path
  argument if not NULL.
  Finds if possible a not too dangerous base.
  TODO: Special handicaps for planes running out of fuel
        IMO should be less restrictive than general H_MAP, H_FOG
*************************************************************************/
static struct tile *find_nearest_airbase(struct unit *punit,
                                         struct tile *start,
                                         int moves,
                                         struct pf_path **path,
                                         int *loss_pcth,
                                         bool immediately)
{
  struct player *pplayer = unit_owner(punit);
  struct pf_parameter parameter;
  struct pf_map *pfm;
  struct tile *tgt = NULL;
  int least_pcth = 201;

  pft_fill_unit_parameter(&parameter, punit);
  if (start) {
    parameter.start_tile = start;
    parameter.moves_left_initially = moves;
  } else {
    moves = parameter.moves_left_initially;
  }
  if (!immediately) { /* Assume no local move bonuses */
    moves += (punit->fuel - 1) * unit_move_rate(punit);
  }
  parameter.omniscience = !has_handicap(pplayer, H_MAP);
  pfm = pf_map_new(&parameter);

  pf_map_move_costs_iterate(pfm, ptile, move_cost, TRUE) {
    if (move_cost > moves) {
      /* Too far! */
      break;
    }

    if (is_airunit_refuel_point(ptile, pplayer, punit)) {
      /* FIXME: adv_danger_at means the enemy is right adjacent,
       * probably there can be other dangers around...
       * How to avoid luring enemy fighters
       * on empty airdromes 2 tiles from a player's city? */
      int loss_here = 0;

      if (!tile_city(ptile)) {
        struct player *towner = tile_owner(ptile);
        if (towner) {
          if (pplayers_at_war(towner, pplayer)) {
            loss_here += 70;
          }
        } else {
          loss_here += 30;
        }
        if (adv_danger_at(punit,  ptile)) {
          loss_here += 100;
        }
      } else if (unit_list_size(ptile->units) <= 0) {
        loss_here += 20;
      }
      if (loss_here == 0) {
        least_pcth = 0;
        tgt = ptile;
        break;
      } else if (loss_here < least_pcth) {
        least_pcth = loss_here;
        tgt = ptile;
      } else if (!tgt) {
        tgt = ptile; /* at least something */
      }
    }
  } pf_map_move_costs_iterate_end;

  if (tgt != NULL) {
    if (loss_pcth != NULL) {
      *loss_pcth = least_pcth;
    }
    if (path) {
      *path = pf_map_path(pfm, tgt);
    }
  }
  pf_map_destroy(pfm);
  return tgt;
}

/**********************************************************************
  Very preliminary estimate for our intent to attack the tile (x, y).
  Used by bombers only.
**********************************************************************/
static bool dai_should_we_air_attack_tile(struct ai_type *ait,
                                          struct unit *punit, struct tile *ptile)
{
  struct city *acity = tile_city(ptile);

  /* For a virtual unit (punit->id == 0), all targets are good */
  /* TODO: There is a danger of producing too many units that will not 
   * attack anything.  Production should not happen if there is an idle 
   * unit of the same type nearby */
  if (acity && punit->id != 0
      && def_ai_city_data(acity, ait)->invasion.occupy == 0
      && !unit_can_take_over(punit)) {
    /* No units capable of occupying are invading */
    log_debug("Don't want to attack %s, although we could",
              city_name_get(acity));
    return FALSE;
  }

  return TRUE;
}

/**********************************************************************
  Returns an estimate for the profit gained through attack.
  Assumes that the victim is within one day's flight
  waste_pcth is assumed chance (0...200) to lose the victorious unit
**********************************************************************/
static int dai_evaluate_tile_for_air_attack(struct unit *punit, 
                                            struct tile *dst_tile,
                                            int waste_pcth)
{
  struct unit *pdefender;
  /* unit costs in shields */
  int balanced_cost, unit_cost, victim_cost = 0;
  /* unit stats */
  int unit_attack, victim_defence;
  /* final answer */
  int profit;
  /* time spent in the air */
  int sortie_time;
#define PROB_MULTIPLIER 100 /* should unify with those in combat.c */

  if (!can_unit_attack_tile(punit, dst_tile)
      || !(pdefender = get_defender(punit, dst_tile))) {
    return 0;
  }

  /* Ok, we can attack, but is it worth it? */

  /* Cost of our unit */
  unit_cost = unit_build_shield_cost(punit);
  /* This is to say "wait, ill unit will get better!" */
  unit_cost = unit_cost * unit_type_get(punit)->hp / punit->hp; 

  /* Determine cost of enemy units */
  victim_cost = stack_cost(punit, pdefender);
  if (0 == victim_cost) {
    return 0;
  }

  /* Missile would die 100% so we adjust the victim_cost -- GB */
  /* Other units might find no way home at some chance */
  victim_cost -= waste_pcth * unit_build_shield_cost(punit) / 200;


  unit_attack = (int) (PROB_MULTIPLIER
                       * unit_win_chance(punit, pdefender));

  victim_defence = PROB_MULTIPLIER - unit_attack;

  balanced_cost = build_cost_balanced(unit_type_get(punit));

  sortie_time = (unit_has_type_flag(punit, UTYF_ONEATTACK) ? 1 : 0);

  profit = kill_desire(victim_cost, unit_attack, unit_cost, victim_defence, 1) 
    - SHIELD_WEIGHTING + 2 * TRADE_WEIGHTING;
  if (profit > 0) {
    profit = military_amortize(unit_owner(punit), 
                               game_city_by_number(punit->homecity),
                               profit, sortie_time, balanced_cost);
    log_debug("%s at (%d, %d) is a worthy target with profit %d", 
              unit_rule_name(pdefender), TILE_XY(dst_tile), profit);
  } else {
    log_debug("%s(%d, %d): %s at (%d, %d) is unworthy with profit %d",
              unit_rule_name(punit), TILE_XY(unit_tile(punit)),
              unit_rule_name(pdefender), TILE_XY(dst_tile), profit);
    profit = 0;
  }

  return profit;
}
  

/**********************************************************************
  Find something to bomb
  Air-units specific victim search
  Returns the want for the best target.  The targets are stored in the
  path and pptile arguments if not NULL.
  if banzai is true, returning from the mission is not considered.
  TODO: take counterattack dangers into account
  TODO: make separate handicaps for air units seeing targets
        IMO should be more restrictive than general H_MAP, H_FOG
*********************************************************************/
static int find_something_to_bomb(struct ai_type *ait, struct unit *punit,
                                  struct pf_path **path, struct tile **pptile,
                                  bool banzai)
{
  struct player *pplayer = unit_owner(punit);
  struct pf_parameter parameter;
  struct pf_map *pfm;
  struct tile *best_tile = NULL;
  int best = 0;
  int safe_path;
  struct unit_type *utype = unit_type_get(punit);
  struct tile *stile = unit_tile(punit);
  /* Can unit (sometimes) bombard.
   * FIXME: Partial support of bombarders yet, just to do something. */
  bool bombarder = utype_has_flag(utype, UTYF_BOMBARDER);
  /* This unit is spent by attack */
  bool missile = uclass_has_flag(utype_class(utype), UCF_MISSILE);
  /* What fuel can we use for way back after the victory
   * (bombarder if actually bombards) */
  int fuel = (utype_has_flag(utype, UTYF_ONEATTACK) || bombarder)
             ? punit->fuel - 1 : punit->fuel;
  int full_fuel = utype_fuel(utype);

  pft_fill_unit_parameter(&parameter, punit);

  parameter.omniscience = !has_handicap(pplayer, H_MAP);
  /* What path from the starting point to the target we consider safe
   * without special checks */
  /* We hope that a base the unit has started from is still there
   * and waits us back (unless we know it does not) */
  if (banzai || missile || fuel << 1 > full_fuel + 1) {
    /* Either we don't care or hope we have enough fuel to reach a base */
    safe_path = parameter.moves_left_initially;
  } else if (1 == fuel
             && (tile_city(stile) || tile_has_refuel_extra(stile, utype)
                 || (unit_transported(punit) && utype_can_freely_load
                       (utype, unit_type_get(unit_transport_get(punit))))
                 || unit_could_load_at(punit, stile))) {
    /* Fighter starts from a city, a base or a landable carrier.
     * Works for not terrain-speed, not tired-attack ones */
    safe_path = parameter.moves_left_initially + 1
      - (game.server.occupychance > 0
         ? 2 * SINGLE_MOVE : SINGLE_MOVE); /* Save it for attack */
    safe_path = MAX(safe_path, 0);
    safe_path /= 2; /* Save it for coming back, hope no slowdown */
  } else {
    safe_path = 0; /* Always check if a base is near. */
    /* Long-range bombers won't distract on noncritical targets
     * on their long way home */
  }
  pfm = pf_map_new(&parameter);

  /* Let's find something to bomb */
  pf_map_move_costs_iterate(pfm, ptile, move_cost, FALSE) {
    if (move_cost >= punit->moves_left) {
      /* Too far! */
      break;
    }

    if (has_handicap(pplayer, H_MAP) && !map_is_known(ptile, pplayer)) {
      /* The target tile is unknown */
      continue;
    }

    if (has_handicap(pplayer, H_FOG) 
        && !map_is_known_and_seen(ptile, pplayer, V_MAIN)) {
      /* The tile is fogged */
      continue;
    }

    if (is_enemy_unit_tile(ptile, pplayer)
        && dai_should_we_air_attack_tile(ait, punit, ptile)
        && can_unit_attack_tile(punit, ptile)) {
      int loss_chance;
      int new_best;

      if (banzai) {
        loss_chance = 0; /* just we don't care */
      } else if (missile) {
        loss_chance = 200; /* we assume that a missile is being refueled */
      } else if (move_cost /* we hope you don't "OneAttack", "Bombarder" */
                 <= (bombarder && is_ocean_tile(ptile)
                     ? safe_path + 1 : safe_path)) {
        loss_chance = 0; /* for bombers, it ends here */
        /* FIXME: In real game, hanging bomber may be an easy target */
      } else {
        /* We should return from the tile we show up on after the attack. */
        /* FIXME: consider attack from another sides, maybe it saves us? */
        /* Cities are usually not takeable by planes and often have > 1 unit */
        bool clear_tile =
         ((!tile_city(ptile) && 1 == unit_list_size(ptile->units))
          || is_stack_vulnerable(ptile))
         && (!bombarder || is_ocean_tile(ptile)); /* can't bombard ocean */
        struct unit_list *cache;
        struct tile *att_from = NULL;
        struct pf_position here;

        pf_map_iter_position(pfm, &here);

        if (is_valid_dir(here.dir_to_here)) {
          att_from = mapstep(ptile, opposite_direction(here.dir_to_here));
        }
        if (!att_from) {
          fc_assert(att_from); /* Always fails */
          /* Won't crash game but will give dummy results. */
          /* By a bug, we may "wait" at the start but it's no problem. */
        }
        if (clear_tile && !game.info.unreachable_protects) {
          unit_list_iterate(ptile->units, aunit) {
            if (!is_unit_reachable_at(aunit, punit, ptile)) {
              clear_tile = FALSE;
              break;
            }
          } unit_list_iterate_end;
        }
        if (clear_tile) {
          /* Kludge to look for all possible ways to retreat */
          /* Might be some extra ZoC when it is not called, but no matter */
          cache = ptile->units;
          ptile->units = unit_list_new(); /* empty */
        }
        if (!clear_tile || game.server.occupychance < 100
            || !can_unit_exist_at_tile(punit, ptile)) {
          int maa = punit->moves_left - move_cost - SINGLE_MOVE;
          maa = MAX(maa, 0);
          if (!find_nearest_airbase(punit, att_from,
                                    maa, NULL, &loss_chance, TRUE)) {
            loss_chance = 200;
          }
        } else {
          loss_chance = 0;
        }
        if (clear_tile) {
          int maa = punit->moves_left - move_cost - 2 * SINGLE_MOVE;
          if (maa >= 0 && game.server.occupychance > 0
              && can_unit_exist_at_tile(punit, ptile)) {
            int occ_loss_chance = 200;
            (void) find_nearest_airbase(punit, ptile,
                                        maa, NULL, &occ_loss_chance, TRUE);
            loss_chance
             = (loss_chance * (100 - game.server.occupychance)
                + occ_loss_chance * game.server.occupychance) / 100;
          }
          /* Kludge revert */
          unit_list_destroy(ptile->units);
          ptile->units = cache;
        }
      }
      new_best = dai_evaluate_tile_for_air_attack
       (punit, ptile, loss_chance);

      if (new_best > best) {
        best_tile = ptile;
        best = new_best;
        log_debug("%s wants to attack tile (%d, %d)", 
                  unit_rule_name(punit), TILE_XY(ptile));
      }
    }
  } pf_map_move_costs_iterate_end;

  /* Return the best values. */
  if (pptile) {
    *pptile = best_tile;
  }
  if (path) {
    *path = best_tile ? pf_map_path(pfm, best_tile) : NULL;
  }

  pf_map_destroy(pfm);
  return best;
} 

/***********************************************************************
  Iterates through reachable cities and appraises them as a possible 
  base for air operations by (air)unit punit.  Returns NULL if not
  found.  The path is stored in the path argument if not NULL.
**********************************************************************/
static struct tile *dai_find_strategic_airbase(struct ai_type *ait,
                                               const struct unit *punit,
                                               struct pf_path **path)
{
  struct player *pplayer = unit_owner(punit);
  struct pf_parameter parameter;
  struct pf_map *pfm;
  struct tile *best_tile = NULL;
  struct city *pcity;
  struct city *home = player_city_by_number(pplayer, punit->homecity);
  struct unit *pvirtual = NULL;
  struct unit_type *utype = unit_type_get(punit);
  int best_worth = 0, target_worth;
  int move_rate = unit_move_rate(punit); /* Assume no local bonuses */
  /* Don't select bases too far. The way is dangerous. */
  int max_moves = punit->moves_left
    + (move_rate * (punit->fuel - (utype_fuel(utype) >> 1)) >> 1);
  int bcb = build_cost_balanced(utype);

  pft_fill_unit_parameter(&parameter, punit);
  parameter.omniscience = !has_handicap(pplayer, H_MAP);
  pfm = pf_map_new(&parameter);
  move_rate = MAX(move_rate, 1);
  pf_map_move_costs_iterate(pfm, ptile, move_cost, FALSE) {
    if (move_cost >= max_moves) {
      break; /* Too far! */
    }

    if (!is_airunit_refuel_point(ptile, pplayer, punit)) {
      continue; /* Cannot refuel here. */
    }

    if ((pcity = tile_city(ptile))
        && def_ai_city_data(pcity, ait)->grave_danger
           > unit_list_size(ptile->units) << 1) {
      /* It used to fly there on any grave_danger, that resulted
       * in piles of unused units */
      best_tile = ptile;
      break; /* Fly there immediately!! */
    }

    if (!pvirtual) {
      pvirtual =
        unit_virtual_create(pplayer, home, utype, punit->veteran);
    }

    unit_tile_set(pvirtual, ptile);
    target_worth = find_something_to_bomb(ait, pvirtual, NULL, NULL, FALSE);
    if (move_cost > punit->moves_left) {
      target_worth
      = military_amortize(pplayer, home, target_worth,
                          (move_cost - punit->moves_left) / move_rate, bcb);
    }
    if (target_worth > best_worth) {
      /* It's either a first find or it's better than the previous. */
      best_worth = target_worth;
      best_tile = ptile;
      /* We can still look for something better. */
    }
  } pf_map_move_costs_iterate_end;

  if (pvirtual) {
    unit_virtual_destroy(pvirtual);
  }

  if (path) {
    /* Stores the path. */
    *path = best_tile ? pf_map_path(pfm, best_tile) : NULL;
  }
  pf_map_destroy(pfm);

  return best_tile;
}

/************************************************************************
  Trying to manage bombers and stuff.
  do {
    If we are in the open and moving intelligently on a valid GOTO, {
      carry on doing it.
    } else {
      try to attack valuable things
    }
    if we are low on fuel {
      if we know a refuel place {
        go refuel
      } else if we are crashing right now {
        do a last chance attack
      }
    } else {
      look for a place we are needed more
    }
  } while we still can do something
  TODO: distant target selection, support for fuel > 2
***********************************************************************/
void dai_manage_airunit(struct ai_type *ait, struct player *pplayer,
                        struct unit *punit)
{
  struct tile *dst_tile = unit_tile(punit);
  /* Loop prevention */
  int moves;
  int id = punit->id;
  struct pf_parameter parameter;
  struct pf_map *pfm;
  struct pf_path *path = NULL;

  CHECK_UNIT(punit);

  if (!is_unit_being_refueled(punit) && punit->activity == ACTIVITY_GOTO) {
    /* We are out in the open on a GOTO, check if it will get us anywhere */
    if (NULL != punit->goto_tile
        && !same_pos(unit_tile(punit), punit->goto_tile)
        && is_airunit_refuel_point(punit->goto_tile, pplayer, punit)
        /* If the site is dangerous, we'd better try reconsidering */
        && !adv_danger_at(punit, punit->goto_tile)) {
      pft_fill_unit_parameter(&parameter, punit);
      pfm = pf_map_new(&parameter);
      path = pf_map_path(pfm, punit->goto_tile);
      bool alive = TRUE;
      if (path) {
        alive = adv_follow_path(punit, path, punit->goto_tile);

        pf_path_destroy(path);
      } else {
        /* Abort the goto. Even if the next turn the way will get free,
         * we maybe won't have enough fuel, then search a closer stripe */
        unit_activity_handling(punit, ACTIVITY_IDLE);
      }
      pf_map_destroy(pfm);
      if (!alive || !punit->moves_left) {
        return;
      }
    }
  }

  /* Try doing attacks considering possible return */
  do {
    moves = punit->moves_left;
    if (dai_do_air_attack(ait, punit, FALSE)) {
      return;
    }
  } while (moves > punit->moves_left);

  if (punit->fuel << 1 <= utype_fuel(unit_type_get(punit)) + 1) {
    /* It's time to refuel */
    if (!(dst_tile
          = find_nearest_airbase(punit, NULL, 0, &path, NULL, FALSE))) {
      if (punit->fuel < 2) {
        /* Helpless. Do a banzai attack. (Maybe we'll clear the way?) */
        if (dai_do_air_attack(ait, punit, TRUE)) {
          return;
        }
      } else {
        /* Don't lose hope! Maybe we'll evacuate later... */
        /* FIXME: find the safest tile around to wait.
         * Alpines on a mountain could protect our bomber from rockets :) */
        def_ai_unit_data(punit, ait)->done = TRUE;
        return;
      }
    } else {
      /* Go to the refueling site */
      bool alive = adv_follow_path(punit, path, dst_tile);
      pf_path_destroy(path);
      if (!alive) {
        return;
      }
    }
  } else if ((dst_tile = dai_find_strategic_airbase(ait, punit, &path))) {
    /* Enough fuel, consider rebasing elsewhere */
    log_debug("%s will fly to (%i, %i) (%s) to fight there",
              unit_rule_name(punit), TILE_XY(dst_tile),
              tile_city(dst_tile) ? city_name_get(tile_city(dst_tile)) : "");
    def_ai_unit_data(punit, ait)->done = TRUE; /* Wait for next turn */
    if (!adv_follow_path(punit, path, dst_tile)) {
      pf_path_destroy(path);
      return; /* The unit died. */
    }
    pf_path_destroy(path);
  } else {
    log_debug("%s cannot find anything to kill and is staying put",
              unit_rule_name(punit));
    def_ai_unit_data(punit, ait)->done = TRUE;
    unit_activity_handling(punit, ACTIVITY_IDLE);
  }

  if ((punit = game_unit_by_number(id)) != NULL) {
    if (punit->moves_left > 0
        && punit->moves_left != moves) {
      /* We have moved this turn, might have ended up stuck out in the fields
       * so, as a safety measure, let's manage again */
      dai_manage_airunit(ait, pplayer, punit);
    } else if (punit->fuel <= 1 && !is_unit_being_refueled(punit)) {
      UNIT_LOG(LOG_DEBUG, punit, "Oops, fallin outta the sky");
    }
  }
}

/*******************************************************************
  Chooses the best available and usable air unit and records it in 
  choice, if it's better than previous choice
  The interface is somewhat different from other ai_choose, but
  that's what it should be like, I believe -- GB
******************************************************************/
bool dai_choose_attacker_air(struct ai_type *ait, struct player *pplayer,
                             struct city *pcity, struct adv_choice *choice)
{
  bool want_something = FALSE;

  /* This AI doesn't know to build planes */
  if (has_handicap(pplayer, H_NOPLANES)) {
    return FALSE;
  }

  /* military_advisor_choose_build does something idiotic, 
   * this function should not be called if there is danger... */
  if (choice->want >= 100 && choice->type != CT_ATTACKER) {
    return FALSE;
  }

  if (!player_knows_techs_with_flag(pplayer, TF_BUILD_AIRBORNE)) {
    return FALSE;
  }

  unit_type_iterate(punittype) {
    struct unit_class *pclass = utype_class(punittype);

    if (pclass->adv.land_move == MOVE_NONE
        || pclass->adv.sea_move == MOVE_NONE
        || uclass_has_flag(pclass, UCF_TERRAIN_SPEED)
        || unit_type_is_losing_hp(pplayer, punittype)) {
      /* We don't consider this a plane */
      continue;
    }

#if AI_NO_FIGHTERS
    /* Temporary hack because pathfinding can't handle Fighters. */
    if (!uclass_has_flag(pclass, UCF_MISSILE) && 1 == utype_fuel(punittype)) {
      continue;
    }
#endif

    if (can_city_build_unit_now(pcity, punittype)) {
      struct unit *virtual_unit = 
	unit_virtual_create(pplayer, pcity, punittype,
                            do_make_unit_veteran(pcity, punittype));
      int profit
        = find_something_to_bomb(ait, virtual_unit, NULL, NULL, FALSE);

      if (profit > choice->want){
        /* Update choice */
        choice->want = profit;
        choice->value.utype = punittype;
        choice->type = CT_ATTACKER;
        choice->need_boat = FALSE;
        want_something = TRUE;
        log_debug("%s wants to build %s (want=%d)",
                  city_name_get(pcity), utype_rule_name(punittype), profit);
      } else {
        log_debug("%s doesn't want to build %s (want=%d)",
                  city_name_get(pcity), utype_rule_name(punittype), profit);
      }
      unit_virtual_destroy(virtual_unit);
    }
  } unit_type_iterate_end;

  return want_something;
}
