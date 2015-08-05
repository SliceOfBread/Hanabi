
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include "Hanabi.h"
#include "ModBot.h"

// Based on AwwBot, this bot only plays the multicolor (very difficult) version of the game
#define NULLCOLOR ORANGE

using namespace Hanabi;

static const bool UseMulligans = true;

template<typename T>
static bool vector_contains(const std::vector<T> &vec, T value)
{
    for (int i=0; i < vec.size(); ++i) {
        if (vec[i] == value) return true;
    }
    return false;
}

CardKnowledge::CardKnowledge()
{
    color_ = -1;
    value_ = -1;
    std::memset(cantBe_, '\0', sizeof cantBe_);
    playable_ = valuable_ = worthless_ = MAYBE;
    clued_ = false;
}

bool CardKnowledge::mustBe(Hanabi::Color color) const { return (this->color_ == color); }
bool CardKnowledge::mustBe(Hanabi::Value value) const { return (this->value_ == value); }
bool CardKnowledge::cannotBe(Hanabi::Card card) const { return cantBe_[card.color][card.value]; }
bool CardKnowledge::cannotBe(Hanabi::Color color) const
{
    if (this->color_ != -1) return (this->color_ != color);
    for (int v = 1; v <= 5; ++v) {
        if (!cantBe_[color][v]) return false;
    }
    return true;
}

bool CardKnowledge::cannotBe(Hanabi::Value value) const
{
    if (this->value_ != -1) return (this->value_ != value);
    for (Color k = RED; k <= MULTI; ++k) {
        if (!cantBe_[k][value]) return false;
    }
    return true;
}

int CardKnowledge::color() const { return this->color_; }
int CardKnowledge::value() const { return this->value_; }
bool CardKnowledge::clued() const { return this->clued_; }

void CardKnowledge::setMode(Hanabi::GameMode mode)
{
    mode_ = mode;
}

void CardKnowledge::setClued(bool gotClued)
{
    clued_ = gotClued;
}

int CardKnowledge::setMustBeMultiOr(Hanabi::Color color)
{
    int tot = 0;
    for (Color k = RED; k <= BLUE; ++k) {
        if (k != color) tot += setCannotBe(k);
    }
    //color_ = color;
    return tot;
}

int CardKnowledge::setMustBe(Hanabi::Color color)
{
    int tot = 0;
    for (Color k = RED; k <= MULTI; ++k) {
        if (k != color) tot += setCannotBe(k);
    }
    color_ = color;
    return tot;
}

int CardKnowledge::setMustBe(Hanabi::Value value)
{
    int tot = 0;
    for (int v = 1; v <= 5; ++v) {
        if (v != value) tot += setCannotBe(Value(v));
    }
    value_ = value;
    return tot;
}

int CardKnowledge::setCannotBeMulti()
{
    int tot = 0;
    for (int v = 1; v <= 5; ++v) {
	if (!cantBe_[MULTI][v]) {
	    tot++;
	    cantBe_[MULTI][v] = true;
	}
    }
    return tot;
}

int CardKnowledge::setCannotBe(Hanabi::Color color)
{
    int tot = 0;
    for (int v = 1; v <= 5; ++v) {
	if (!cantBe_[color][v]) {
	    tot++;
	    cantBe_[color][v] = true;
	}
    }
    return tot;
}

int CardKnowledge::setCannotBe(Hanabi::Value value)
{
    int tot = 0;
    for (Color k = RED; k <= MULTI; ++k) {
        if (!cantBe_[k][value]) {
	    tot++;
	    cantBe_[k][value] = true;
	}
    }
    return tot;
}

void CardKnowledge::setIsPlayable(const Server& server, bool knownPlayable)
{
    for (Color k = RED; k <= MULTI; ++k) {
        int playableValue = server.pileOf(k).size() + 1;
        for (int v = 1; v <= 5; ++v) {
            if (this->cantBe_[k][v]) continue;
            if ((v == playableValue) != knownPlayable) {
                this->cantBe_[k][v] = true;
            }
        }
    }
    this->playable_ = (knownPlayable ? YES : NO);
}

void CardKnowledge::setIsValuable(const ModBot &bot, const Server& server, bool knownValuable)
{
    for (Color k = RED; k <= MULTI; ++k) {
        for (int v = 1; v <= 5; ++v) {
            if (this->cantBe_[k][v]) continue;
            if (bot.isValuable(server, Card(k,v)) != knownValuable) {
                this->cantBe_[k][v] = true;
            }
        }
    }
    this->valuable_ = (knownValuable ? YES : NO);
}

void CardKnowledge::setIsWorthless(const ModBot &bot, const Server& server, bool knownWorthless)
{
    for (Color k = RED; k <= MULTI; ++k) {
        for (int v = 1; v <= 5; ++v) {
            if (this->cantBe_[k][v]) continue;
            if (bot.isWorthless(server, Card(k,v)) != knownWorthless) {
                this->cantBe_[k][v] = true;
            }
        }
    }
    this->worthless_ = (knownWorthless ? YES : NO);
}

void CardKnowledge::update(const Server &server, const ModBot &bot, bool useMyEyesight)
{
    int color = this->color_;
    int value = this->value_;

    if (useMyEyesight) goto complicated_part;

  repeat_loop:

    // if card can't be 5 colors, it must be 6th color
    if (color == -1) {
        for (Color k = RED; k <= MULTI; ++k) {
            if (this->cannotBe(k)) continue;
            else if (color == -1) color = k;
            else { color = -1; break; }
        }
        if (color != -1) this->setMustBe(Color(color));
    }

    // if card can't be 4 number, it must be 5th number
    if (value == -1) {
        for (int v = 1; v <= 5; ++v) {
            if (this->cannotBe(Value(v))) continue;
            else if (value == -1) value = v;
            else { value = -1; break; }
        }
        if (value != -1) this->setMustBe(Value(value));
    }

  complicated_part:

    assert(color == this->color_);
    assert(value == this->value_);

    /* Rule out any cards that have been completely played and/or discarded. */
    if (value == -1 || color == -1) {
        bool restart = false;
        for (Color k = RED; k <= MULTI; ++k) {
            for (int v = 1; v <= 5; ++v) {
                if (this->cantBe_[k][v]) continue;
                const int total = (v == 1 ? 3 : (v == 5 ? 1 : 2));
                const int played = bot.playedCount_[k][v];
                const int held = (useMyEyesight ? bot.eyesightCount_[k][v] : bot.locatedCount_[k][v]);
                assert(played+held <= total);
                if (played+held == total) {
                    this->cantBe_[k][v] = true;
                    restart = true;
                }
            }
        }
        if (restart) goto repeat_loop;
    }

    if (true) {
        bool yesP = false, noP = false;
        bool yesV = false, noV = false;
        bool yesW = false, noW = false;
        for (Color k = RED; k <= MULTI; ++k) {
            int playableValue = server.pileOf(k).size() + 1;
            for (int v = 1; v <= 5; ++v) {
                if (this->cantBe_[k][v]) continue;
                if (v < playableValue) {
                    noP = true;
                    noV = true;
                    yesW = true;
                } else if (v == playableValue) {
                    yesP = true;
                    if (!yesV || !noV) {
                        const int count = Card(k,v).count();
			if (bot.playedCount_[k][v] == count-1) {
			    yesV = true;
			} else {
			    noV = true;
			}
                    }
                    noW = true;
                } else {
                    noP = true;
                    if (!yesV || !noV) {
                        if (bot.isValuable(server, Card(k,v))) {
			    yesV  = true;
			} else {
			    noV = true;
			}
                    }
                    if (!yesW || !noW) {
                        if (bot.isWorthless(server, Card(k,v))) {
			    yesW = true;
			} else {
			    noW = true;
			}
                    }
                }
            }
            if (yesP && yesV && yesW) break;
        }
        assert(yesP || noP);
        assert(yesV || noV);
        assert(yesW || noW);
        this->playable_ = (yesP ? (noP ? MAYBE : YES) : NO);
        this->valuable_ = (yesV ? (noV ? MAYBE : YES) : NO);
        this->worthless_ = (yesW ? (noW ? MAYBE : YES) : NO);
    }

    if (worthless_ == YES) assert(valuable_ == NO);
    if (worthless_ == YES) assert(playable_ == NO);
}

Hint::Hint()
{
    fitness = -1;
    color = -1;
    value = -1;
    to = -1;
}

void Hint::give(Server &server)
{
    assert(to != -1);
    if (color != -1) {
        server.pleaseGiveColorHint(to, Color(color));
    } else if (value != -1) {
        server.pleaseGiveValueHint(to, Value(value));
    } else {
        assert(false);
    }
}

ModBot::ModBot(int index, int numPlayers, int handSize, Hanabi::GameMode mode)
{
    me_ = index;
    mode_ = mode;
    //TODONEXT
    handKnowledge_.resize(numPlayers);
    for (int i=0; i < numPlayers; ++i) {
        handKnowledge_[i].resize(handSize);
    }

    maxEval_ = 20;
    if (numPlayers >= 4) {
	maxEval_ = 30;
    }

    for (int i=0; i < 5; ++i) {
	playClued_[i] = false;
	discardClued_[i] = false;
    }
    ignoreCluesIfDiscardValuable_ = false;
    priorPlayerDiscarded_ = false;
    std::memset(playedCount_, '\0', sizeof playedCount_);
}

bool ModBot::isPlayable(const Server &server, Card card) const
{
    const int playableValue = server.pileOf(card.color).size() + 1;
    return (card.value == playableValue);
}

bool ModBot::isValuable(const Server &server, Card card) const
{
    /* A card which has not yet been played, and which is the
     * last of its kind, is valuable. */
    if (playedCount_[card.color][card.value] != card.count()-1) return false;
    return !this->isWorthless(server, card);
}

bool ModBot::isWorthless(const Server &server, Card card) const
{
    const int playableValue = server.pileOf(card.color).size() + 1;
    if (card.value < playableValue) return true;
    if (true) {
        /* If all the red 4s are in the discard pile, then the red 5 is worthless.
         * But doing this check all the time apparently lowers ModBot's average score! */
        while (card.value > playableValue) {
            --card.value;
            if (playedCount_[card.color][card.value] == card.count()) return true;
        }
    }
    return false;
}


/* Could "knol" be playable, if it were known to be of value "value"? */
bool ModBot::couldBePlayableWithValue(const Server &server, const CardKnowledge &knol, int value) const
{
    if (value < 1 || 5 < value) return false;
    if (knol.playable() != MAYBE) return (knol.playable() == YES);
    for (Color k = RED; k <= MULTI; ++k) {
        Card card(k, value);
        if (knol.cannotBe(card)) continue;
        if (this->isPlayable(server, card))
            return true;
    }
    return false;
}

/* Could "knol" be valuable, if it were known to be of value "value"? */
bool ModBot::couldBeValuableWithValue(const Server &server, const CardKnowledge &knol, int value) const
{
    if (value < 1 || 5 < value) return false;
    if (knol.valuable() != MAYBE) return false;
    for (Color k = RED; k <= MULTI; ++k) {
        Card card(k, value);
        if (knol.cannotBe(card)) continue;
        if (this->isValuable(server, card))
            return true;
    }
    return false;
}

void ModBot::invalidateKnol(int player_index, int card_index)
{
    /* The other cards are shifted down and a new one drawn at the end. */
    std::vector<CardKnowledge> &vec = handKnowledge_[player_index];
    for (int i = card_index; i+1 < vec.size(); ++i) {
        vec[i] = vec[i+1];
    }
    vec.back() = CardKnowledge();
}

void ModBot::seePublicCard(const Card &card)
{
    int &entry = this->playedCount_[card.color][card.value];
    entry += 1;
    assert(1 <= entry && entry <= card.count());
}

void ModBot::updateEyesightCount(const Server &server)
{
    std::memset(this->eyesightCount_, '\0', sizeof this->eyesightCount_);

    const int numPlayers = handKnowledge_.size();
    for (int p=0; p < numPlayers; ++p) {
        if (p == me_) {
            for (int i=0; i < myHandSize_; ++i) {
                CardKnowledge &knol = handKnowledge_[p][i];
                if (knol.color() != -1 && knol.value() != -1) {
                    this->eyesightCount_[knol.color()][knol.value()] += 1;
                }
            }
        } else {
            const std::vector<Card> hand = server.handOfPlayer(p);
            for (int i=0; i < hand.size(); ++i) {
                const Card &card = hand[i];
                this->eyesightCount_[card.color][card.value] += 1;
            }
        }
    }
}

bool ModBot::updateLocatedCount(const Hanabi::Server &server)
{
    int newCount[Hanabi::NUMCOLORS][5+1] = {};

    for (int p=0; p < handKnowledge_.size(); ++p) {
        for (int i=0; i < handKnowledge_[p].size(); ++i) {
            CardKnowledge &knol = handKnowledge_[p][i];
            int k = knol.color();
            if (k != -1) {
                int v = knol.value();
                if (v != -1) {
                    newCount[k][v] += 1;
                }
            }
        }
    }

    if (std::memcmp(this->locatedCount_, newCount, sizeof newCount) != 0) {
        std::memcpy(this->locatedCount_, newCount, sizeof newCount);
        return true;  /* there was a change */
    }
    return false;
}


void ModBot::noValuableWarningWasGiven(const Hanabi::Server &server, int from)
{
    /* Something just happened that wasn't a warning. If what happened
     * wasn't a hint to the guy expecting a warning, then he can safely
     * deduce that his card isn't valuable enough to warn about. */

    /* The rules are different when there are no cards left to draw,
     * or when valuable-warning hints can't be given. */
    if (server.cardsRemainingInDeck() == 0) return;
    if (server.hintStonesRemaining() == 0) return;

    const int playerExpectingWarning = (from + 1) % handKnowledge_.size();
    //const int discardIndex = this->nextDiscardIndex(server, playerExpectingWarning);

    //if (discardIndex != -1) {
       // handKnowledge_[playerExpectingWarning][discardIndex].setIsValuable(*this, server, false);
    //}
}

void ModBot::pleaseObserveBeforeMove(const Server &server)
{
    assert(server.whoAmI() == me_);

    myHandSize_ = server.sizeOfHandOfPlayer(me_);

    for (int p=0; p < handKnowledge_.size(); ++p) {
        const int numCards = server.sizeOfHandOfPlayer(p);
        assert(handKnowledge_[p].size() >= numCards);
        handKnowledge_[p].resize(numCards);
    }
    
    std::memset(this->locatedCount_, '\0', sizeof this->locatedCount_);
    this->updateLocatedCount(server);
    do {
        for (int p=0; p < handKnowledge_.size(); ++p) {
            const int numCards = handKnowledge_[p].size();
            for (int i=0; i < numCards; ++i) {
                CardKnowledge &knol = handKnowledge_[p][i];
		knol.setMode(mode_);
                knol.update(server, *this, false);
            }
        }
    } while (this->updateLocatedCount(server));

    this->updateEyesightCount(server);

    lowestPlayableValue_ = 6;
    for (Color color = RED; color <= MULTI; ++color) {
        lowestPlayableValue_ = std::min(lowestPlayableValue_, server.pileOf(color).size()+1);
    }

    for (Color k = RED; k <= MULTI; ++k) {
        for (int v = 1; v <= 5; ++v) {
            assert(this->locatedCount_[k][v] <= this->eyesightCount_[k][v]);
        }
    }
}

void ModBot::pleaseObserveBeforeDiscard(const Hanabi::Server &server, int from, int card_index)
{
    //this->noValuableWarningWasGiven(server, from);

    // If we didn't need to discard then next player(s) have no playable cards that are not
    // already known playable
    /*
    if ((server.hintStonesRemaining() != 0) &&
	(!handLocked_[from])) {
	int nextPlayer = (from + 1) % server.numPlayers();
	for (int c=0; c < server.sizeOfHandOfPlayer(nextPlayer); c++) {
	    CardKnowledge &knol = handKnowledge_[nextPlayer][c];
	    if (//knol.clued() &&
		(knol.playable() != YES)) {
		knol.setIsPlayable(server, false);
	    }
	}
    }
    */

    

    assert(server.whoAmI() == me_);
    Card card = server.activeCard();
    if (isValuable(server, card)) {
	// if we are discarding something valuable, ignore all future clues
	for (int i=0; i < 5; ++i) {
	    playClued_[i] = false;
	    discardClued_[i] = false;
	}
    }

    this->seePublicCard(card);
    if (((from + 1) % server.numPlayers() == me_) &&
	(isValuable(server, card))) {
	// if prior player discarded and that card is now valuable then we should not discard.
	// Here we set flag that will ensure we play or clue.
	priorPlayerDiscarded_ = true;
	priorDiscardColor_ = card.color;
	priorDiscardValue_ = card.value;
    }
    this->invalidateKnol(from, card_index);
}

void ModBot::pleaseObserveBeforePlay(const Hanabi::Server &server, int from, int card_index)
{
    assert(server.whoAmI() == me_);
    priorPlayerDiscarded_ = false;

    //this->noValuableWarningWasGiven(server, from);

    Card card = server.activeCard();

    assert(handKnowledge_[from][card_index].worthless() != YES);
    if (handKnowledge_[from][card_index].valuable() == YES) {
        /* We weren't wrong about this card being valuable, were we? */
        assert(this->isValuable(server, card));
    }

    this->seePublicCard(card);
    this->invalidateKnol(from, card_index);
}

void ModBot::pleaseObserveColorHint(const Hanabi::Server &server, int from, int to, Color color, const std::vector<int> &card_indices)
{
    assert(server.whoAmI() == me_);
    priorPlayerDiscarded_ = false;
    // a color clue to LH1 corresponds to an eval = value + 5
    // a color clue to LH2 corresponds to an eval = value + 15

    int numPlayers = server.numPlayers();
    if ((from + 1) % numPlayers == to) {
	observeEval(server, from, color + 5);
    } else if ((from + 2) % numPlayers == to) {
	observeEval(server, from, color + 15);
    } else {
	observeEval(server, from, color + 25);
    }

    const int toHandSize = server.sizeOfHandOfPlayer(to);

    // set each card as mustBe/clued or cantBe
    for (int i=0; i < toHandSize; ++i) {
	CardKnowledge &knol = handKnowledge_[to][i];
	if (vector_contains(card_indices, i)) {
	    knol.setClued(true);
	    knol.setMustBeMultiOr(color);
	    knol.update(server, *this, false);
	} else {
	    knol.setCannotBe(color);
	    knol.setCannotBeMulti();
	}
    }

}

void ModBot::pleaseObserveValueHint(const Hanabi::Server &server, int from, int to, Value value, const std::vector<int> &card_indices)
{
    assert(server.whoAmI() == me_);
    priorPlayerDiscarded_ = false;

    // a value clue to LH1 corresponds to an eval = value - 1
    // a value clue to LH2 corresponds to an eval = value + 9
    // a value clue to LH2 corresponds to an eval = value + 14

    int numPlayers = server.numPlayers();
    if ((from + 1) % numPlayers == to) {
	observeEval(server, from, value - 1);
    } else if ((from + 2) % numPlayers == to) {
	observeEval(server, from, value + 9);
    } else {
	observeEval(server, from, value + 19);
    }

    const int toHandSize = server.sizeOfHandOfPlayer(to);

    // set each card as mustBe/clued or cantBe
    for (int i=0; i < toHandSize; ++i) {
	CardKnowledge &knol = handKnowledge_[to][i];
	if (vector_contains(card_indices, i)) {
	    knol.setClued(true);
	    knol.setMustBe(value);
	    knol.update(server, *this, false);
	} else {
	    knol.setCannotBe(value);
	}
    }


}

void ModBot::observeEval(const Hanabi::Server &server, int from, int evalInput)
{
    int numPlayers = server.numPlayers();
    int eval[5];
    int totalEval = evaluateAllHands(server, from, me_, eval);
    totalEval = totalEval % maxEval_;

    // Except for the clue giver (from) we need to calculate the eval for
    // each player's own hand (since we can't see our own hand).
    if (from != me_) {
	// everyone except "from player" is effectively receiving this clue
	eval[me_] = evalInput - totalEval;
	if (eval[me_] < 0) eval[me_] += maxEval_;
    }

    // We can now determine what the players have been clued (play or discard).
    // Order of precedence dictates a player with a "play" clue will play or
    // sometimes clue to save a problem. However, LH1 cannot possibly clue after this
    // to save something in LH2's hand (because they'd get the same clue again).
    // If LH1 discards something valuable when we think they should play, 
    // ignore the rest of this clue. (Actually, for now, if anyone ever discards
    // something valuable, we'll ignore all future clues.)
    if (server.hintStonesRemaining() == 1) {
	//ignoreCluesIfDiscardValuable_ = true;
    }
    for (int i=1; i<numPlayers; i++) {
	int p = (from + i) % numPlayers;
	playClued_[p] = false;
	discardClued_[p] = false;
	// If our own eval is >=15, make sure it is for us. If not, set it correctly.
	if (eval[p] > 14) {
	    int otherp = (int((eval[p] - 5) / 5) + from) % numPlayers;
	    if (otherp != p) {
		// need to swap evals of p and otherp
		int tmp = eval[p] - (eval[p] % 5);
		eval[p] = eval[p] % 5;
		eval[otherp] += tmp;
	    }
	}
	cluedIndex_[p] = eval[p] % 5;
	if ((eval[p] > 9) && (eval[p] < 15)) {
	    
	} else if (eval[p] < 5) {
	    playClued_[p] = true;
	    if (server.hintStonesRemaining() != 1) {
		// this is definitely a playable card 
		CardKnowledge &knol = handKnowledge_[p][eval[p] % 5];
		knol.setIsPlayable(server, true);
		knol.update(server, *this, false);
		playClued_[p] = true;
	    }
	} else {
	    discardClued_[p] = true;
	}
    }
}

void ModBot::pleaseObserveAfterMove(const Hanabi::Server &server)
{
    assert(server.whoAmI() == me_);
    int player = server.activePlayer();
    playClued_[player] = false;
    discardClued_[player] = false;

}

bool ModBot::botCanPlay(Hanabi::GameMode mode)
{
    if (mode==VERYDIFFICULT) return true;
    return false;
}

int ModBot::bestCardToPlay(Server &server)
{
    
    /* Try to find a card that nobody else knows I know is playable
     * (because they don't see what I see). Let's try to get that card
     * out of my hand before someone "helpfully" wastes a hint on it.
     */
    CardKnowledge eyeKnol[5];
    for (int i=0; i < myHandSize_; ++i) {
        eyeKnol[i] = handKnowledge_[me_][i];
        eyeKnol[i].update(server, *this, /*useMyEyesight=*/true);
    }

    int best_index = -1;
    int best_fitness = 0;
    for (int i=0; i < myHandSize_; ++i) {
        if (eyeKnol[i].playable() != YES) continue;

        /* How many further plays are enabled by this play?
         * Rough heuristic: 5 minus its value. Notice that this
         * gives an extra-high fitness to cards that are "known playable"
         * but whose color/value is unknown (value() == -1).
         * TODO: If both red 4s were discarded, then the red 3 doesn't open up any plays.
	 * TODO: Give higher value to cards if the card that follows it is known playable
	 * in someone else's hand
         */
        int fitness = (6 - eyeKnol[i].value());
	// If there are no clues, maybe better to play 5 to get a clue back
	// TODO don't do this if it's the last round of the game and playing something else
	// helps more.
        if (fitness >= best_fitness) {
            best_index = i;
            best_fitness = fitness;
        }
    }

    return best_index;
}

bool ModBot::maybePlayLowestPlayableCard(Server &server)
{
    if (playClued_[me_]) {
	server.pleasePlay(cluedIndex_[me_]);
	return true;
    }
    
    // Don't play something I might find below if I've been forced to discard.
    if (discardClued_[me_]) {
	return false;
    }

    // If I find a card to play, play it 
    // This can happen when a player clues and a subsequent player plays making a card
    // in this players hand playable, and this player knows enough about the card to play it.
    int best_index = bestCardToPlay(server);
    if (best_index != -1) {
        server.pleasePlay(best_index);
        return true;
    }

    return false;
}

bool ModBot::maybeDontDoubleDiscard(Server &server)
{
    // check if the prior player discarded something that now might mean
    // my discard (if any) is valuable. Don't discard if that is the case.
    if (!priorPlayerDiscarded_) return false;
    
    Card priorDiscard = Card(Color(priorDiscardColor_), priorDiscardValue_);
    // is that card now valuable?
    if (!isValuable(server, priorDiscard)) return false;

    // Do I even have something marked for discard?
    if (!discardClued_[me_]) return false;

    // Do I know the card I would discard is NOT the same card as prior discard?
    CardKnowledge eyeKnol;
    eyeKnol = handKnowledge_[me_][cluedIndex_[me_]];
    eyeKnol.update(server, *this, /*useMyEyesight=*/true);
    if (eyeKnol.cannotBe(priorDiscard)) return false;

    // At this point we know prior player discarded something valuable and
    // I may have the same card marked for discard, so I will clue! (if possible)
    return maybeGiveHelpfulHint(server, false);
}


int ModBot::handEval(const Server &server, int partner) const
{

    if ((server.hintStonesRemaining() == 1) &&
	(server.cardsRemainingInDeck() > 1)) {
	return discardPriorityHandEval(server, partner, false);
    }

    const std::vector<Card> partners_hand = server.handOfPlayer(partner);
    int numPlayers = server.numPlayers();

    // If already have card clued play, if it is still playable, return its index
    if (playClued_[partner]) {
	// already have a playable clued. Check if it is still playable
	Card card = partners_hand[cluedIndex_[partner]];
	if (server.pileOf(card.color).nextValueIs(card.value)) {
	    // Note: if this same card is/will be marked playable by previous player
	    // that will be handled in the calling routine
	    return cluedIndex_[partner];
	} // else card is no longer playable!
    }

    // No clued play or it's not playable. Find oldest playable, if any.
    for (int c=0; c < partners_hand.size() ; ++c) {
	Card card = partners_hand[c];
	if (server.pileOf(card.color).nextValueIs(card.value)) {
	    // Note: if this same card is/will be marked playable by previous player
	    // that will be handled in the calling routine
	    return c;
	}
    }

    // No playables. :( Find oldest worthless card, if any
    for (int c=0; c < partners_hand.size() ; ++c) {
	if (isWorthless(server, partners_hand[c])) {
	    return (5 + c);
	} else {
	    // Also check for duplicate cards in same hand
	    for (int cc=c + 1; cc < partners_hand.size(); ++cc) {
		if (partners_hand[c] == partners_hand[cc]) {
		    return (5 + c);
		}
	    }
	}
    }

    Hanabi::Value bestValuableValue = ONE;
    int bestValuableIndex = -1;
    Hanabi::Value bestNonvaluableValue = ONE;
    int bestNonvaluableIndex = -1;
    // No playables. No worthless. 
    // Find oldest, nonvaluable 4
    // else nonvaluable 3
    // else nonvaluable 2
    // else (getting ugly now) oldest 5
    // else oldest 4
    // else oldest 3
    // else oldest 2 (Yikes! a hand full of non-playable, nonreplacable 2's!)
    for (int c=0; c < partners_hand.size() ; ++c) {
	if (isValuable(server, partners_hand[c])) {
	    if (partners_hand[c].value > bestValuableValue) {
		bestValuableValue = partners_hand[c].value;
		bestValuableIndex = c;
	    }
	} else {
	    if (partners_hand[c].value > bestNonvaluableValue) {
		bestNonvaluableValue = partners_hand[c].value;
		bestNonvaluableIndex = c;
	    }
	}
    }
    if (bestNonvaluableValue > ONE) return (5 + bestNonvaluableIndex);
    return (10 + bestValuableIndex);


}

int ModBot::discardPriorityHandEval(const Server &server, int partner, bool excludePlay) const
{

    const std::vector<Card> partners_hand = server.handOfPlayer(partner);
    int numPlayers = server.numPlayers();

    // Worthless is top priority for discard
    for (int c=0; c < partners_hand.size() ; ++c) {
	if (isWorthless(server, partners_hand[c])) {
	    return (5 + c);
	} else {
	    // Also check for duplicate cards in same hand
	    for (int cc=c + 1; cc < partners_hand.size(); ++cc) {
		if (partners_hand[c] == partners_hand[cc]) {
		    return (5 + c);
		}
	    }
	}
    }

    // No worthless. 
    // Find oldest, nonvaluable 4
    // else nonvaluable 3
    // else nonvaluable 2
    // else playable (see below)
    // else (getting ugly now) oldest 5
    // else oldest 4
    // else oldest 3
    // else oldest 2 (Yikes! a hand full of non-playable, nonreplacable 2's!)
    Hanabi::Value bestValuableValue = ONE;
    int bestValuableIndex = -1;
    Hanabi::Value bestNonvaluableValue = ONE;
    int bestNonvaluableIndex = -1;
    for (int c=0; c < partners_hand.size() ; ++c) {
	if (isValuable(server, partners_hand[c])) {
	    if (partners_hand[c].value > bestValuableValue) {
		bestValuableValue = partners_hand[c].value;
		bestValuableIndex = c;
	    }
	} else {
	    if (partners_hand[c].value > bestNonvaluableValue) {
		bestNonvaluableValue = partners_hand[c].value;
		bestNonvaluableIndex = c;
	    }
	}
    }

    int playIndex = -1;
    if (!excludePlay) {
	// If already have card clued play, if it is still playable, return its index
	if (playClued_[partner]) {
	    // already have a playable clued. Check if it is still playable
	    Card card = partners_hand[cluedIndex_[partner]];
	    if (server.pileOf(card.color).nextValueIs(card.value)) {
		// Note: if this same card is/will be marked playable by previous player
		// that will be handled in the calling routine
		playIndex = cluedIndex_[partner];
	    } // else card is no longer playable!
	}

	// No clued play or it's not playable. Find oldest playable, if any.
	for (int c=0; c < partners_hand.size() ; ++c) {
	    Card card = partners_hand[c];
	    if (server.pileOf(card.color).nextValueIs(card.value)) {
		// Note: if this same card is/will be marked playable by previous player
		// that will be handled in the calling routine
		playIndex = c;
	    }
	}
    }

    if (bestNonvaluableValue > ONE) return (5 + bestNonvaluableIndex);
    if (playIndex >= 0) return playIndex;
    return (10 + bestValuableIndex);


}


int ModBot::evaluateAllHands(const Server &server, int skipMe, int skipAlso, int *eval) const
{
    int numPlayers = server.numPlayers();

    int totalEval = 0;
    //int eval[numPlayers];
    bool haveSeenDiscard = false;
    bool forceDiscard = false;
    // evaluate each of my partners hands according to the eval rules (see eval section)
    // Note that eval does not check for 2 players (or more) getting the same "play" clue.
    // That is checked in the inner loop.

    for (int i=1; i < numPlayers; i++) {
	// We don't evaluate my own (clue giver's) hand 
	int p = (skipMe + i) % numPlayers;
	if (p == skipAlso) continue;
	eval[p] = handEval(server, p);
	if (eval[p] < 5) {
	    // this player is getting a "play" clue
	    // check if card is same as a previous player
	    for (int ii=1; ii < i; ii++) {
		int priorp = (skipMe + ii) % numPlayers;
		if (priorp == skipAlso) continue;
		if (eval[priorp] < 5) {
		    // if player p (who playes after priorp) would play same card as p
		    // increase eval. If dup is in hand of:
		    // LH2, increase by 15
		    // LH3, increase by 20
		    // LH4, increase by 25
		    if (server.handOfPlayer(priorp)[eval[priorp]] == server.handOfPlayer(p)[eval[p]]) {
			eval[p] += (i*5) + 5;
			break;
		    }
		}
	    }
	} else if (eval[p] >= 10) {
	    // This is a bad thing to discard. If an earlier plaer hasn't discarded yet, we may
	    // have to force LH1 to discard :(
	    if (!haveSeenDiscard) forceDiscard = true;
	} else {
	    haveSeenDiscard = true;
	}
	totalEval += eval[p];
    }

    if ((server.hintStonesRemaining() == 1) && 
	    forceDiscard &&
	    (skipAlso == -1)) {
	// we gave our last hint and even though we are using discard priority
	// no discards were clued. That means we need to force LH1 to discard something
	// valuable. Fortunately, this shouldn't happen too often (we hope).
	int discEval = discardPriorityHandEval(server, (skipMe + 1) % numPlayers, true);
	if (discEval >= 10) discEval -= 5;
	totalEval -= eval[(skipMe + 1) % numPlayers]; // delete previous eval
	totalEval += discEval;	// and add the new one
    }
    return totalEval;
}

bool ModBot::maybeGiveValuableWarning(Server &server) 
{
    // can't give hint without stones!
    if (server.hintStonesRemaining() == 0) return false;

    // will next player discard something valuable?
    int nextPlayer = (me_ + 1) % server.numPlayers();
    if (!discardClued_[nextPlayer]) return false;
    if (isValuable(server, server.handOfPlayer(nextPlayer)[cluedIndex_[nextPlayer]])) {
	// nextPlayer is poised to discard a valuable card
	// Can we reclue his hand to fix it?
	int eval = handEval(server, nextPlayer);
	if ((eval >= 5) && ((eval % 5)==cluedIndex_[nextPlayer])) {
	    // new clue doesn't change things 
	    // Hmmm, should current player discard automatically or 
	    // just continue normal progression (we'll try this one)
	    return false; 
	}
	// following should always return true since we have hint stones
	return maybeGiveHelpfulHint(server, true);
    }
    // next players discard is not valuable
    return false;
}


bool ModBot::maybeOverClueNextPlayer(Server &server)
{
    // if there are only a few cards remaining and any player is unclued and
    // we have lots of clues (more clues than cards left), then clue!
    if (server.hintStonesRemaining() < server.cardsRemainingInDeck()) return false;

    int numPlayers = server.numPlayers();
    for (int i=1; i<numPlayers; i++) {
	int p = (me_ + i) % server.numPlayers();
	if (!playClued_[p] && !discardClued_[p]) {
	    return maybeGiveHelpfulHint(server, false);
	}
    }
    
    return false; 
    
}

bool ModBot::maybeGiveHelpfulHint(Server &server, bool forceClue)
{
    if (server.hintStonesRemaining() == 0) return false;
    int numPlayers = server.numPlayers();

    int eval[5];
    int totalEval = evaluateAllHands(server, me_, -1, eval);
    
    if ((numPlayers == 2) && (totalEval > 9)) {
	// other player has nothing to play or discard
	if (forceClue) {
	    // other player has all vluable/non-playable cards. Adjust eval so
	    // if he discards, it will be his least hurtful
	    totalEval = totalEval % 10;
	} else {
	    // in 2P, unless forced, we will not clue a hand with all
	    // valuable/non-playables
	    return false;
	}
    }

    // Convert eval into a hint!

    Hint hint;
    hint.to = (me_ + 1) % numPlayers;
    totalEval = totalEval % maxEval_;
    // if totalEvalue < 5, clue LH1 value totalEval+1
    // if totalEval>=5, <10, clue LH1 color(totalEval % 5)
    // if totalEval>=10, <15, clue LH2 value (totalEval % 5)+1 (only happens with 3+P)
    // if totalEval>=15, <20, clue LH2 color(totalEval % 5) (only happens with 3+P)
    // if totalEval>=20, <25, clue LH3 value (totalEval % 5)+1 (only happens with 5P)
    if (totalEval > 19) {
	hint.to = (me_ + 3) % numPlayers;
    } else if (totalEval > 9) {
	hint.to = (me_ + 2) % numPlayers;
    }
    if ((totalEval % 10) < 5) {
	hint.value = (totalEval % 5) + 1;
    } else {
	hint.color = totalEval % 5;
    }

    /* Give the hint. */
    hint.give(server);
    return true;
}

bool ModBot::maybePlayMysteryCard(Server &server)
{
    if (!UseMulligans) return false;
    //if (clueWaiting_[(me_+1) % server.numPlayers()]) return false; 

    const int table[4] = { -99, 1, 1, 3 };
    if (server.cardsRemainingInDeck() <= table[server.mulligansRemaining()]) {
        /* We could temporize, or we could do something that forces us to
         * draw a card. If we got here, temporizing has been rejected as
         * an option; so let's do something that forces us to draw a card.
         * At this point, we might as well try to play something random
         * and hope we get lucky. */
        //for (int i = handKnowledge_[me_].size() - 1; i >= 0; --i) {
        for (int i = 0; i < handKnowledge_[me_].size(); ++i) {
            CardKnowledge eyeKnol = handKnowledge_[me_][i];
            eyeKnol.update(server, *this, /*useMyEyesight=*/true);
            assert(eyeKnol.playable() != YES);  /* or we would have played it already */
            if (eyeKnol.playable() == MAYBE) {
                server.pleasePlay(i);
                return true;
            }
        }
    }
    return false;
}

void ModBot::pleaseMakeMove(Server &server)
{
    assert(server.whoAmI() == me_);
    assert(server.activePlayer() == me_);
    assert(UseMulligans || !server.mulligansUsed());
    
    for (int p=0; p < handKnowledge_.size(); ++p) {
	const int numCards = handKnowledge_[p].size();
	for (int i=0; i < numCards; ++i) {
	    CardKnowledge &knol = handKnowledge_[p][i];
	    knol.setMode(mode_);
	}
    }

    if (maybeGiveValuableWarning(server)) return;
    if (maybePlayLowestPlayableCard(server)) return;
    if (maybeDontDoubleDiscard(server)) return;
    if (maybeOverClueNextPlayer(server)) return;
    

    if (!server.discardingIsAllowed()) {
	// discarding not allowed means we have 8 clues.
	maybeGiveHelpfulHint(server, true);	// this will ALWAYS clue;
	return;
    } else {

	if (discardClued_[me_]) {
	    server.pleaseDiscard(cluedIndex_[me_]);
	} else {
	    if (maybeGiveHelpfulHint(server, true)) return;
	    // only get here if no clues left
	    if (maybePlayMysteryCard(server)) return;
	    server.pleaseDiscard(0);
	}
	return;
    }
    // We'll only get here when 1 card or less remains and its was better to clue plays
    // then force discard. So now we have a player with no play or discard clued.
}

// $Log: ModBot.cc,v $
//
//
