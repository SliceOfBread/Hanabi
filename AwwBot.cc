
#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cstring>
#include "Hanabi.h"
#include "AwwBot.h"

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

void CardKnowledge::setIsValuable(const AwwBot &bot, const Server& server, bool knownValuable)
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

void CardKnowledge::setIsWorthless(const AwwBot &bot, const Server& server, bool knownWorthless)
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

void CardKnowledge::update(const Server &server, const AwwBot &bot, bool useMyEyesight)
{
    int color = this->color_;
    int value = this->value_;

    if (mode_ == Hanabi::NORMAL) this->setCannotBe(MULTI);

    if (useMyEyesight) goto complicated_part;

  repeat_loop:

    // if card can't be 4 colors, it must be 5th color
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

AwwBot::AwwBot(int index, int numPlayers, int handSize, Hanabi::GameMode mode)
{
    me_ = index;
    mode_ = mode;
    handKnowledge_.resize(numPlayers);
    for (int i=0; i < numPlayers; ++i) {
        handKnowledge_[i].resize(handSize);
    }
    for (int i=0; i < 5; ++i) {
	handLocked_[i] = false;
	clueWaiting_[i] = false;
    }
    std::memset(playedCount_, '\0', sizeof playedCount_);
}

bool AwwBot::isPlayable(const Server &server, Card card) const
{
    const int playableValue = server.pileOf(card.color).size() + 1;
    return (card.value == playableValue);
}

bool AwwBot::isValuable(const Server &server, Card card) const
{
    /* A card which has not yet been played, and which is the
     * last of its kind, is valuable. */
    if (playedCount_[card.color][card.value] != card.count()-1) return false;
    return !this->isWorthless(server, card);
}

bool AwwBot::isWorthless(const Server &server, Card card) const
{
    const int playableValue = server.pileOf(card.color).size() + 1;
    if (card.value < playableValue) return true;
    if (true) {
        /* If all the red 4s are in the discard pile, then the red 5 is worthless.
         * But doing this check all the time apparently lowers AwwBot's average score! */
        while (card.value > playableValue) {
            --card.value;
            if (playedCount_[card.color][card.value] == card.count()) return true;
        }
    }
    return false;
}


/* Could "knol" be playable, if it were known to be of value "value"? */
bool AwwBot::couldBePlayableWithValue(const Server &server, const CardKnowledge &knol, int value) const
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
bool AwwBot::couldBeValuableWithValue(const Server &server, const CardKnowledge &knol, int value) const
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

void AwwBot::invalidateKnol(int player_index, int card_index)
{
    /* The other cards are shifted down and a new one drawn at the end. */
    std::vector<CardKnowledge> &vec = handKnowledge_[player_index];
    for (int i = card_index; i+1 < vec.size(); ++i) {
        vec[i] = vec[i+1];
    }
    vec.back() = CardKnowledge();
}

void AwwBot::seePublicCard(const Card &card)
{
    int &entry = this->playedCount_[card.color][card.value];
    entry += 1;
    assert(1 <= entry && entry <= card.count());
}

void AwwBot::updateEyesightCount(const Server &server)
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

bool AwwBot::updateLocatedCount(const Hanabi::Server &server)
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

int AwwBot::nextDiscardIndex(const Hanabi::Server &server, int to) const
{
    // This function finds next card we would discard. From highest to lowest priority:
    // oldest known worthless card
    // oldest unclued card but we have a playable
    // oldest unclued card (nothing to play)
    // 
    // 
    // return 32+index of lowest worthless card
    // return 24+index of lowest unclued (but we have something playable)
    // return 16 if found playable but no discardables
    // return 8 +index of lowest unclued (nothing playable)
    // return 0 if nothing "allowed" to be discarded
    const int numCards = handKnowledge_[to].size();
    int best_fitness = 0;
    int best_index = 0;
    int foundPlayable = 0;
    if (clueWaiting_[to]) foundPlayable = 16;
    for (int i=0; i < numCards; ++i) {
        const CardKnowledge &knol = handKnowledge_[to][i];
        if (knol.playable() == YES) foundPlayable = 16;
        if (knol.worthless() == YES) return (i+32);  
        if (knol.valuable() == YES) continue;  /* we should never discard this card */
        if (knol.clued()) continue;  /* we should never discard this card */

        if ((best_index == 0) && (!knol.clued())) {
	    // if we don't find a worthless card, this will hold the oldest unclued card
            best_index = i+8;
        }
    }
    return best_index + foundPlayable;
}

trivalue AwwBot::isCluedElsewhere(const Hanabi::Server &server, int partner, int cardNum) const
{

    trivalue returnVal = NO;
    std::vector<Card> otherHand = server.handOfPlayer(partner);
    Color color = otherHand[cardNum].color;
    Value value = otherHand[cardNum].value;

    for (int p=0; p < handKnowledge_.size() ; ++p) {
	if (p!=me_) {
	    otherHand = server.handOfPlayer(p);
	}
	for (int c=0; c < handKnowledge_[p].size() ; ++c) {
	    if ((p==partner) && (c==cardNum)) continue;	// don't check against itself
	    if (p==me_) {
		// I can only look at what I've been clued
		if (handKnowledge_[p][c].mustBe(color)) {
		    // If card fully know, return w/ YES
		    if (handKnowledge_[p][c].mustBe(value)) return YES;
		    // We know color is right. If value is NOT wrong and card is clued, mark as maybe
		    if ((!handKnowledge_[p][c].cannotBe(value)) &&
			(handKnowledge_[p][c].clued())) returnVal = MAYBE;
		} else if (handKnowledge_[p][c].mustBe(value)) {
		    // We don't know the color
		    // if it COULD be this color and it's clued, mark as maybe
		    if ((!handKnowledge_[p][c].cannotBe(color)) &&
			(handKnowledge_[p][c].clued())) returnVal = MAYBE;
		}
	    } else {
		// if not me, I can look directly at cards
		if ((otherHand[c].color == color) && (otherHand[c].value == value) &&
		    (handKnowledge_[p][c].clued())) {
		    // the same card is clued elsewhere
		    return YES;
		} else if ((handKnowledge_[p][c].color() == color) &&
		           (handKnowledge_[p][c].value() == value)) {
		    // although not clued, the card is known playable elsewhere
		    return YES;
		}
	    }
	}
    }

    return returnVal;
}

void AwwBot::noValuableWarningWasGiven(const Hanabi::Server &server, int from)
{
    /* Something just happened that wasn't a warning. If what happened
     * wasn't a hint to the guy expecting a warning, then he can safely
     * deduce that his card isn't valuable enough to warn about. */

    /* The rules are different when there are no cards left to draw,
     * or when valuable-warning hints can't be given. */
    if (server.cardsRemainingInDeck() == 0) return;
    if (server.hintStonesRemaining() == 0) return;

    const int playerExpectingWarning = (from + 1) % handKnowledge_.size();
    const int discardIndex = this->nextDiscardIndex(server, playerExpectingWarning);

    if (discardIndex != -1) {
        handKnowledge_[playerExpectingWarning][discardIndex].setIsValuable(*this, server, false);
    }
}

void AwwBot::pleaseObserveBeforeMove(const Server &server)
{
    assert(server.whoAmI() == me_);

    myHandSize_ = server.sizeOfHandOfPlayer(me_);

    for (int p=0; p < handKnowledge_.size(); ++p) {
        const int numCards = server.sizeOfHandOfPlayer(p);
        assert(handKnowledge_[p].size() >= numCards);
        handKnowledge_[p].resize(numCards);
    }
    
    // Did this player earlier receive a play clue that we delayed marking playable?
    int ap = server.activePlayer();
    if (clueWaiting_[ap]) {
	handKnowledge_[ap][clueWaitingIndex_[ap]].setIsPlayable(server, true);
	clueWaiting_[ap] = false;
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

void AwwBot::pleaseObserveBeforeDiscard(const Hanabi::Server &server, int from, int card_index)
{
    //this->noValuableWarningWasGiven(server, from);

    assert(server.whoAmI() == me_);
    Card card = server.activeCard();
    this->seePublicCard(card);
    this->invalidateKnol(from, card_index);
}

void AwwBot::pleaseObserveBeforePlay(const Hanabi::Server &server, int from, int card_index)
{
    assert(server.whoAmI() == me_);

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

void AwwBot::pleaseObserveColorHint(const Hanabi::Server &server, int from, int to, Color color, const std::vector<int> &card_indices)
{
    assert(server.whoAmI() == me_);

    // A RED clue means save your chop!
    // a BLUE, GREEN, etc. hint means play your oldest, 2nd oldest, etc.
    // For 4/5 player       // For 2/3 player game 
    // clue col O Y G B     // clue col   O Y G B
    // play crd 3 2 1 0     // play crd 4 3 2 1 0

    const int playableIndex = 4 - color;
    const int toHandSize = server.sizeOfHandOfPlayer(to);

    bool seenUnclued = false;
    // set each card as mustBe/clued or cantBe
    for (int i=0; i < toHandSize; ++i) {
	CardKnowledge &knol = handKnowledge_[to][i];
	if ((color == RED) && !seenUnclued && !knol.clued()) {
	    seenUnclued=true;
	    knol.setClued(true);
	    knol.setIsValuable(*this, server, true);
	}
	if (vector_contains(card_indices, i)) {
	    knol.setClued(true);
	    knol.setMustBe(color);
	    knol.update(server, *this, false);
	} else {
	    knol.setCannotBe(color);
	}
    }

    // set indexed card as playable
    int numPlayers = server.numPlayers();
    if (color != RED) {
	CardKnowledge &knol = handKnowledge_[to][playableIndex];
	if (to != (from + 1) % numPlayers) {
	    // if we give clue to someone other than LH1, it might be a finesse
	    clueWaiting_[to] = true;
	    clueWaitingIndex_[to] = playableIndex;
	    knol.setClued(true);
	    int lhp = (from + 1) % server.numPlayers();
	    handLocked_[lhp] = true;
	    if (server.whoAmI() != to) {
		Card possibleFinesseCard = server.handOfPlayer(to)[playableIndex];
		if (isPlayable(server, possibleFinesseCard)) {
		    // Since the card is currently playable it is not being used to finesse
		    handLockedIndex_[lhp] = lockCardToPlay(server, lhp);
		} else {
		    // the card is not playable and so it must be finessing LH1
		    handLockedIndex_[lhp] = whatFinessePlay(server, lhp, possibleFinesseCard);
		}
	    } else {
		// the player receiving a possible finesse clue cannot look at their own hand
		// Also, they don't care if it is a finesse or not. They just wait for LH1 to
		// do something.
		handLockedIndex_[lhp] = lockCardToPlay(server, lhp);
	    }
	    // Also, lock in-between player's play, if any
	    do {
		lhp = (lhp + 1) % server.numPlayers();
		if (lhp == to) break;
		handLocked_[lhp] = true;
		handLockedIndex_[lhp] = lockCardToPlay(server, lhp);
	    } while (true);
	} else {
	    if (knol.playable() != NO) {
		knol.setIsPlayable(server, true);
		knol.setClued(true);
		knol.update(server, *this, false);
	    }
	}
    }
}

void AwwBot::pleaseObserveValueHint(const Hanabi::Server &server, int from, int to, Value value, const std::vector<int> &card_indices)
{
    assert(server.whoAmI() == me_);

    // A 5 clue to LH1 means "I have nothing better to do"
    // A 5 clue to LH2 means "LH1 and LH2 have the same next discard"
    // A 1, 2, 3 or 4 clue means your oldest, next oldest, etc is playable
    // For 4/5 player       // For 2/3 player game 
    // clue val 1 2 3 4     // clue val 1 2 3 4
    // play crd 3 2 1 0     // play crd 4 3 2 1 0

    const int toHandSize = server.sizeOfHandOfPlayer(to);
    const int playableIndex = toHandSize - value;

    // If we are giving a 5 clue to LH2, we will need this info
    const int lh1 = (from + 1) % server.numPlayers();
    int lh1discard = this->nextDiscardIndex(server, lh1) % 8;
    int lh2discard = this->nextDiscardIndex(server, to) % 8;

    // set each card as mustBe/clued or cantBe
    for (int i=toHandSize-1; i >= 0; --i) {
	CardKnowledge &knol = handKnowledge_[to][i];
	if (vector_contains(card_indices, i)) {
	    knol.setClued(true);
	    knol.setMustBe(value);
	    knol.update(server, *this, false);
	} else {
	    knol.setCannotBe(value);
	}
    }

    // set indexed card as playable
    if (value != 5) {
	CardKnowledge &knol = handKnowledge_[to][playableIndex];
	if (to != (from + 1) % server.numPlayers()) {
	    // if we give clue to LH2, it might be a finesse on LH1 so delay the clue
	    clueWaiting_[to] = true;
	    clueWaitingIndex_[to] = playableIndex;
	    knol.setClued(true);
	    // Also, lock LH1's play
	    int lhp = (from + 1) % server.numPlayers();
	    handLocked_[lhp] = true;
	    if (server.whoAmI() != to) {
		Card possibleFinesseCard = server.handOfPlayer(to)[playableIndex];
		if (isPlayable(server, possibleFinesseCard)) {
		    // Since the card is currently playable it is not being used to finesse
		    handLockedIndex_[lhp] = lockCardToPlay(server, lhp);
		} else {
		    // the card is not playable and so it must be finessing LH1
		    handLockedIndex_[lhp] = whatFinessePlay(server, lhp, possibleFinesseCard);
		}
	    } else {
		// the player receiving a possible finesse clue cannot look at their own hand
		// Also, they don't care if it is a finesse or not. They just wait for LH1 to
		// do something.
		handLockedIndex_[lhp] = lockCardToPlay(server, lhp);
	    }
	    // Also, lock in-between player's play, if any
	    do {
		lhp = (lhp + 1) % server.numPlayers();
		if (lhp == to) break;
		handLocked_[lhp] = true;
		handLockedIndex_[lhp] = lockCardToPlay(server, lhp);
	    } while (true);
	} else if (knol.playable() != NO) {
	    knol.setIsPlayable(server, true);
	    knol.setClued(true);
	    knol.update(server, *this, false);
	}
    } else {
	if (to == (from + 2) % server.numPlayers()) {
	    // We are giving a 5 clue to LH2 which means LH1 chop == LH2 chop
	    CardKnowledge &lh1knol = handKnowledge_[to][lh1discard];
	    // I'd like to set lh1 card worthless but this might cause issues as it technically isn't
	    CardKnowledge &knol = handKnowledge_[to][lh2discard];
	    knol.setClued(true);
	    if (server.whoAmI() == from) {
		assert(server.handOfPlayer(lh1)[lh1discard] == server.handOfPlayer(to)[lh2discard]);
	    }
	    if (server.whoAmI() != lh1) {
		// everyone except lh1 can update based on lh1's card
		Card lh1card = server.handOfPlayer(lh1)[lh1discard];
		knol.setMustBe(lh1card.color);
		knol.setMustBe(lh1card.value);
		knol.update(server, *this, false);
	    } else {
		Card lh2card = server.handOfPlayer(to)[lh2discard];
		knol.setMustBe(lh2card.color);
		knol.setMustBe(lh2card.value);
		knol.update(server, *this, false);
	    }
	}

    }

}

void AwwBot::pleaseObserveAfterMove(const Hanabi::Server &server)
{
    assert(server.whoAmI() == me_);
    int player = server.activePlayer();
    handLocked_[player] = false;
}

bool AwwBot::botCanPlay(Hanabi::GameMode mode)
{
    if (mode==NORMAL) return true;
    if (mode==TRICKY) return true;
    return false;
}

int AwwBot::lockCardToPlay(const Hanabi::Server &server, int player) const
{
    // unlike bestCardToPlay, this finds the next card a player will play based SOLELY on
    // public knowledge at the time a lock is placed. This is done so everyone knows what will
    // be played and future cards can be clued based on this assumption.
    // First check if this player has a clueWaiting because that is the card they would play
    if (clueWaiting_[player]) return clueWaitingIndex_[player];
    
    CardKnowledge eyeKnol[5];
    for (int i=0; i < server.sizeOfHandOfPlayer(player); ++i) {
        eyeKnol[i] = handKnowledge_[player][i];
    }

    int best_index = -1;
    int best_fitness = 0;
    for (int i=0; i < server.sizeOfHandOfPlayer(player); ++i) {
        if (eyeKnol[i].playable() != YES) continue;

        /* How many further plays are enabled by this play?
         * Rough heuristic: 5 minus its value. Notice that this
         * gives an extra-high fitness to cards that are "known playable"
         * but whose color/value is unknown (value() == -1).
	 * TODO: Give higher value to cards if the card that follows it is 
	 * in someone else's hand
         */
        int fitness = (6 - eyeKnol[i].value());
        if (fitness >= best_fitness) {
            best_index = i;
            best_fitness = fitness;
        }
    }

    return best_index;
}

int AwwBot::bestCardToPlay(Server &server)
{
    // if our hand is locked, play whatever we planned to when it got locked
    if (handLocked_[me_]) return handLockedIndex_[me_];
    
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
	if ((server.hintStonesRemaining() < 1) && (eyeKnol[i].value() == 5)) fitness += 1;
        if (handKnowledge_[me_][i].playable() != YES) fitness += 100;
        if (fitness >= best_fitness) {
            best_index = i;
            best_fitness = fitness;
        }
    }

    return best_index;
}

bool AwwBot::maybePlayLowestPlayableCard(Server &server)
{
    int best_index = bestCardToPlay(server);
    /* If I found a card to play, play it. */
    if (best_index != -1) {
        server.pleasePlay(best_index);
        return true;
    }

    return false;
}

bool AwwBot::maybeDiscardWorthlessCard(Server &server)
{
    /* Try to find a card that nobody else knows I know is worthless
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
        if (eyeKnol[i].worthless() != YES) continue;

        /* Prefer cards that I've deduced are worthless. */
        int fitness = (handKnowledge_[me_][i].worthless() == YES) ? 1 : 2;
        if (fitness > best_fitness) {
            best_index = i;
            best_fitness = fitness;
        }
    }

    /* If I found a card to discard, discard it. */
    if (best_index != -1) {
        server.pleaseDiscard(best_index);
        return true;
    }

    return false;
}

Hint AwwBot::bestHintForPlayer(const Server &server, int partner) const
{
    assert(partner != me_);
    const std::vector<Card> partners_hand = server.handOfPlayer(partner);
    
    int lockCardIndex = -1;
    Card lockCard = Card(RED, 1);
    if (((me_ + 2) % server.numPlayers()) == partner) {
	// we are cluing LH2
	// since we will lock LH1, see what LH1 will play, if anything
	int lh1 = (me_ + 1) % server.numPlayers();
	lockCardIndex = lockCardToPlay(server, lh1 );
	if (lockCardIndex >= 0) {
	    lockCard = server.handOfPlayer(lh1)[lockCardIndex];
	}
	   
    }

    bool is_really_playable[5];
    for (int c=0; c < partners_hand.size(); ++c) {
        is_really_playable[c] =
            server.pileOf(partners_hand[c].color).nextValueIs(partners_hand[c].value);
    }

    // For 4/5 player       // For 2/3 player game 
    // clue val 1 2 3 4     // clue val 1 2 3 4
    // clue col O Y G B     // clue col   O Y G B
    // play crd 3 2 1 0     // play crd 4 3 2 1 0
    // cluing 5 means "we have nothing else to do"
    // cluing R means mark oldest unclued as "clued"

    Hint best_so_far;
    best_so_far.to = partner;

    // 
    int partnersHandSize =partners_hand.size();
    for (int c=0; c < partnersHandSize; ++c) {
	Color colorClue = Color(4 - c);
	Value valueClue = Value(partnersHandSize - c);
	int colorFitness = -1;
	int valueFitness = -1;
	if (is_really_playable[c]) {
	    // Do not clue if this clue goes to LH2 and and LH1 will be locked into playing same card
	    if ((lockCardIndex >= 0) &&
		(lockCard == partners_hand[c])) continue;
	    // if already marked playable, don't clue again
	    if (handKnowledge_[partner][c].playable() == YES) continue;

	    trivalue cluedElsewhere = isCluedElsewhere(server, partner, c);
	    if (colorClue != RED) {
		colorFitness = 26 - partners_hand[c].value;
		if (cluedElsewhere == YES)  colorFitness -= 5;
		// See what else this color clue does
		for (int oc=0; oc < partnersHandSize; ++oc) {
		    if (c==oc) continue;
		    CardKnowledge eknol;
		    eknol = handKnowledge_[partner][oc];
		    if (eknol.color() != -1) continue;
		    bool alreadyPlayable = (eknol.playable() == YES);
		    bool alreadyValuable = (eknol.valuable() == YES);
		    bool alreadyWorthless = (eknol.worthless() == YES);
		    bool alreadyClued = eknol.clued();
		    if (partners_hand[oc].color == colorClue) {
			eknol.setMustBe(colorClue);
			//eknol.update(server, *this, false);
		    } else {
			eknol.setCannotBe(colorClue);
		    }
		    eknol.update(server, *this, false);
		    if (partners_hand[oc] != partners_hand[c]) {
			// this is not a duplicate card
			if (partners_hand[oc].color == colorClue) {
			    // if we clue colorClue, we'd also be cluing another card of that color
			    if (!alreadyPlayable && (partners_hand[oc].color == colorClue)) colorFitness += 2;
			    if (!alreadyValuable && !alreadyPlayable &&
				(isValuable(server, partners_hand[oc]))) {
				// side effect of clue would be to save a valuable card
				colorFitness += 15;
			    } else if (!alreadyValuable && !alreadyPlayable &&
				(eknol.valuable() != NO) ) {
				colorFitness += 1;
			    }
			    if (!alreadyWorthless &&
				(eknol.worthless() == YES)) {
				colorFitness += 1;
			    }
			    if (!alreadyWorthless && !alreadyPlayable &&
				(eknol.mustBe(partners_hand[oc].value))) {
				// card would be fully clued with this clue
				colorFitness += 5;
			    }
			}

		    } else {
			// we have the same card more than once
			if (!alreadyClued) {
			    if (partners_hand[oc].color == colorClue) {
				// card not already clued and is a duplicate that would be clued
				// that is not helpful
				colorFitness -= 2;
			    }
			}
		    }
		}
	    }
	    if (valueClue != 5) {
		valueFitness = 26 - partners_hand[c].value;
		if (cluedElsewhere == YES) {
		    valueFitness -= 2;
		}
		// See what else this value clue does
		for (int oc=0; oc < partnersHandSize; ++oc) {
		    if (c==oc) continue;
		    CardKnowledge eknol;
		    eknol = handKnowledge_[partner][oc];
		    if (eknol.value() != -1) continue;
		    bool alreadyPlayable = (eknol.playable() == YES);
		    bool alreadyValuable = (eknol.valuable() == YES);
		    bool alreadyWorthless = (eknol.worthless() == YES);
		    bool alreadyClued = eknol.clued();
		    //int cantsSet = eknol.setMustBe(valueClue);
		    if (partners_hand[oc].value == valueClue) {
			eknol.setMustBe(valueClue);
			//eknol.update(server, *this, false);
		    } else {
			eknol.setCannotBe(valueClue);
		    }
		    eknol.update(server, *this, false);
		    if (partners_hand[oc] != partners_hand[c]) {
			// this is not a duplicate card
			if (partners_hand[oc].value == valueClue) {
			    // if we clue valueClue, we'd also be cluing another card of that value
			    if (!alreadyPlayable && (partners_hand[oc].value == valueClue)) valueFitness += 2;
			    if (!alreadyValuable && !alreadyPlayable &&
				(isValuable(server, partners_hand[oc]))) {
				// side effect of clue would be to save a valuable card
				if (valueClue != 5) valueFitness += 15;
			    } else if (!alreadyValuable && !alreadyPlayable &&
				(eknol.valuable() != NO) ) {
				valueFitness += 1;
			    }
			    if (!alreadyWorthless &&
				(eknol.worthless() == YES)) {
				valueFitness += 1;
			    }
			    if (!alreadyWorthless && !alreadyPlayable &&
				(eknol.mustBe(partners_hand[oc].color))) {
				// card would be fully clued with this clue
				valueFitness += 5;
			    }
			}

		    } else {
			// we have the same card more than once
			if (!alreadyClued && 
			    (partners_hand[oc].value == valueClue)) {
			    // card not already clued and is a duplicate that would be clued
			    // that is not helpful
			    valueFitness -= 2;
			}
			    
		    }
		}
	    }

	} else {
	    // theoretically, we can clue something not playable if 
	    // A) it is 100% clear the clued card can't be played right away AND
	    // B) it will give us some other playable (or something really valuable)
	    
	}
	if (colorFitness > best_so_far.fitness) {
	    best_so_far.fitness = colorFitness;
            best_so_far.color = colorClue;
            best_so_far.value = -1;
	}
	if (valueFitness > best_so_far.fitness) {
	    best_so_far.fitness = valueFitness;
            best_so_far.color = -1;
            best_so_far.value = valueClue;
	}
    }

    return best_so_far;
}

int AwwBot::whatFinessePlay(const Server &server, int partner, Card fcard) const
{
    // if this player was finessed using fcard, what card would he play?
    
    bool foundPart = false;
    int partIndex;
    bool foundUnknown = false;
    int unknownIndex = -1;
    for (int c = handKnowledge_[partner].size() - 1; c >= 0; c--) {
	const CardKnowledge &knol = handKnowledge_[partner][c];
	if ((knol.color() == fcard.color) &&
	    (knol.value() == fcard.value)) {
	    return c;
	}
	if (!foundPart) {
	    if ((knol.color() == fcard.color) || (knol.value() == fcard.value)) {
		if (!knol.cannotBe(fcard)) {
		    foundPart = true;
		    partIndex = c;
		    continue;
		}
	    }
	}
 	if (!foundUnknown) {
	    if ((knol.color() == -1) &&
		(knol.value() == -1)) {
		foundUnknown = true;
		unknownIndex = c;
	    }
	}
    }
    if(foundPart) return partIndex;
    
    return unknownIndex;
	
}

bool AwwBot::maybeFinesseNextPlayer(Server &server, int distanceToFinesse, int distanceForFinesse)
{
    if ((handLocked_[me_]) && (handLockedIndex_[me_] != -1)) return false;

    const int numPlayers = handKnowledge_.size();
    // the distance variables indicate how far left our partner is. e.g. 1 == LH1
    // pToFinesse is the player we are trying to get to play some card without cluing
    // pForFinesse is the player we are cluing to accomplish the above.
    int pToFinesse = (me_ + distanceToFinesse) % numPlayers;
    int pForFinesse = (me_ + distanceForFinesse) % numPlayers;
    if ((pToFinesse == me_) || (pForFinesse == me_)) return false;
    
    /* Sometimes we just can't give a hint. */
    if (server.hintStonesRemaining() == 0) return false;

    bool banned[5] = {false, false, false, false, false};

    // Does pToFinesse have any playable cards?
    // TODO if pToFinesse already has known playable, it would be better to first check
    // if he had !known playable but finessable. For 4P or more, it would also be better
    // lock this hand and check for a later finesse.
    int nextValueForPile[5];
    for (Color color=RED; color <= MULTI; color++) {
	nextValueForPile[color] = 1 + server.pileOf(color).size();
    }
    // next check all players between me and pToFinesse to ensure they will play a card if locked and
    // we base any finesse on that lock
    for (int i=2; i < distanceForFinesse; i++) {
	int lp = (me_ + i) % numPlayers;
	int tmpLock = lockCardToPlay(server, lp);
	if (tmpLock < 0) {
	    // player doesn't have a playable to lock
	    // We do not want player to discard something that is not known worthless.
	    // If he did, that could make another card valuable and then it may need saving and
	    // that causes issues.
	    int nextDiscard = nextDiscardIndex(server, lp);
	    if ((nextDiscard < 16)) {
		// Note: Could also check if nextDiscard is worthless (even if not KNOWN worthless
		return false;
	    }
	} else {
	    // if already banned, 2 inbetween players will play same lock card!
	    if (banned[server.handOfPlayer(lp)[tmpLock].color]) return false;
	    // player will play tmpLock card so ban finesse of that color
	    banned[server.handOfPlayer(lp)[tmpLock].color] = true;
	}
    }
    // TODONEXT - determine when to recursively call maybeFinesseNextPlayer
    // if pForFinesse did not have following card to any of pToFinesse, check pToFinesse, pForFinesse+1
    // if pToFinesse did not have finessable card, check pToFinesse+1, pToFinesse+2







		
    bool isFinessable[5];
    bool havePotentialFinesse = false;
    const std::vector<Card> partners_hand = server.handOfPlayer(pToFinesse);
    for (int c=0; c < partners_hand.size(); ++c) {
	isFinessable[c] = false;
	if (!banned[partners_hand[c].color]) {
	    isFinessable[c] =
		server.pileOf(partners_hand[c].color).nextValueIs(partners_hand[c].value);
	    //havePotentialFinesse = true;
	}
    }
    // Didn't find a playable card.
    // TODONEXT - We could check pToFinesse + 1
    //if (!havePotentialFinesse) return false;
    
    // We have at least one currently playable card.
    // See which, if any, have the potential to be finessed.
    havePotentialFinesse = false;
    for (int c=0; c < partners_hand.size(); ++c) {
	if (isFinessable[c]) {
	    if (partners_hand[c].value == FIVE) {
		// Can't finesse a 5
		isFinessable[c] = false;
	    } else {
		Card fcard = Card(partners_hand[c].color, partners_hand[c].value + 1); 
		int findex = whatFinessePlay(server, pToFinesse, fcard);
		if (findex != c) {
		    // if we clued the card that would finesse this, something else would get played
		    isFinessable[c] = false;
		} else {
		    havePotentialFinesse = true;
		}
	    }
	}
    }
    // Didn't find a finessable card.
    // TODONEXT - We could check pToFinesse + 1
    if (!havePotentialFinesse) return false;
    
    // We have at least one potentially finessable card.
    // Next check if card that could finesse it is in a future hand
    for (int c=0; c < partners_hand.size(); ++c) {
	if (isFinessable[c]) { 
	    Card fcard = Card(partners_hand[c].color, partners_hand[c].value + 1); 
	    // check hand of pForFinesse for the card that follows partners finessable card
	    const std::vector<Card> fplayer_hand = server.handOfPlayer(pForFinesse);
	    for (int fc=0; fc < fplayer_hand.size(); fc++) {
		if (fplayer_hand[fc] == fcard) {
		    // we found a card that can be used to finesse partner
		    // TODO We could rate the fitness of each possible finesse
		    // 		and each possible clue.
		    int colorFitness = -1;
		    int valueFitness = -1;
		    Color colorClue = Color(4 - fc);
		    Value valueClue = Value(fplayer_hand.size() - fc);
		    for (int oc=0; oc < fplayer_hand.size(); ++oc) {
			if (fc==oc) continue;
			if (colorClue != RED) {
			    colorFitness++;
			    if (fplayer_hand[oc].color == colorClue) {
				if (!handKnowledge_[pForFinesse][oc].mustBe(colorClue)) {
				    colorFitness++;
				    if (!handKnowledge_[pForFinesse][oc].clued() &&
					isValuable(server, fplayer_hand[oc])) {
					colorFitness += 4;
				    }
				}
			    }
			}
			if (valueClue != FIVE) {
			    valueFitness++;
			    if (fplayer_hand[oc].value == valueClue) {
				if (!handKnowledge_[pForFinesse][oc].mustBe(valueClue)) {
				    valueFitness++;
				    if (!handKnowledge_[pForFinesse][oc].clued() &&
					isValuable(server, fplayer_hand[oc])) {
					valueFitness += 4;
				    }
				}
			    }
			}
		    }
		    if (colorFitness > valueFitness) {
			// clue as color
			server.pleaseGiveColorHint(pForFinesse, colorClue);
			return true;
		    } else if (valueFitness > -1) {
			// clue as value
			server.pleaseGiveValueHint(pForFinesse, valueClue);
			return true;
		    } else {
			assert(false);
		    }
		}
	    }
	    // we did not find the follow up card for any of LH1's finessable cards
	    // We can check the next player's hand if:
	    // -the next player is not me
	    // -the current player either has a known playable or will discard something worthless
	    // (discarding something non-worthless likely makes a card valuable which could then be danger)
	    // All this checking is done in the recursive call
	    if (maybeFinesseNextPlayer(server, distanceToFinesse, distanceForFinesse+1)) return true;
	}
    }

    return false;
}

bool AwwBot::maybeGiveValuableWarning(Server &server, int playerDistance)
{
    if (handLocked_[me_]) return false;
    
    /* Sometimes we just can't give a hint. */
    if (server.hintStonesRemaining() == 0) return false;

    const int numPlayers = handKnowledge_.size();
    const int player_to_warn = (me_ + playerDistance) % numPlayers;

    /* Is the player to our left just about to discard a card
     * that is really valuable? */
    int discardIndex = this->nextDiscardIndex(server, player_to_warn);
    
    // Does player have a worthless card or nothing unclued?
    if ((discardIndex > 31) || ((discardIndex % 16)==0))  return false;
    
    bool doubleDiscardPossible = false;
    int lh2;
    int lh2discard;
    // Player MIGHT discard
    Card targetCard = server.handOfPlayer(player_to_warn)[discardIndex % 8];
    if (!this->isValuable(server, targetCard)) {
        /* The target card isn't actually valuable. Good. */
	// Unless both LH1 and LH2 have the same discard card
	if ((numPlayers > 2) &&		    // we have at least 3 players
		(playerDistance == 1)) {    // checking LH1
	    lh2 = (player_to_warn + 1) % numPlayers;
	    lh2discard = this->nextDiscardIndex(server, lh2);
	    // Does player have a worthless card or nothing unclued?
	    if ((lh2discard > 31) || ((lh2discard % 16)==0))  return false;
	    Card lh2target = server.handOfPlayer(lh2)[lh2discard % 8];
	    if (targetCard == lh2target) {
		// Yikes, both players might discard
		doubleDiscardPossible = true;
	    } else {
		// they are not the same cards
		return false;
	    }
	} else {
	    // we are not in a double discard scenario
	    return false;
	}
    }

    // One last thing to check. If the player already has a known playable card 
    //	there is no need to clue warning
    // TODO: Do we want to look ahead for problems
    if (discardIndex > 23) return false;

    discardIndex = discardIndex % 8;

    /* Oh no! Warn him before he discards it! */
    assert(handKnowledge_[player_to_warn][discardIndex].playable() != YES);
    assert(handKnowledge_[player_to_warn][discardIndex].valuable() != YES);
    assert(handKnowledge_[player_to_warn][discardIndex].worthless() != YES);
    
    Hint bestHint = bestHintForPlayer(server, player_to_warn);
    if (bestHint.fitness > 0) {
        /* Excellent; we found a hint that will cause him to play a card
         * instead of discarding. */
        bestHint.give(server);
        return true;
    }

    // We have not found a way around preventing LH1 from discarding
    // If we are in double discard territory see if we can fix things in LH2's hand
    if (doubleDiscardPossible) {
	// does LH2 have a playable?
	if (lh2discard > 23) return false;

	bestHint = bestHintForPlayer(server, lh2);
	if (bestHint.fitness > 0) {
	    /* Excellent; we found a hint that will cause lh2 to play a card
	     * instead of discarding. */
	    bestHint.give(server);
	    return true;
	}

	// Although it is possible that either LH1 or LH2 will not discard (they could clue)
	// we will hint LH2 as FIVE to indicate his discard == LH1's discard
	server.pleaseGiveValueHint(lh2, FIVE);
	return true;
    }


    /* Otherwise, we'll have to give a warning. */
    if (targetCard.value == lowestPlayableValue_) {
        assert(server.pileOf(targetCard.color).nextValueIs(targetCard.value));
    } else {
        assert(targetCard.value > lowestPlayableValue_);
    }
    if ((targetCard.value == 5) && (playerDistance == 1)) {
	server.pleaseGiveValueHint(player_to_warn, FIVE);
    } else {
	server.pleaseGiveColorHint(player_to_warn, RED);
    }
    return true;
}

bool AwwBot::maybeGiveHelpfulHint(Server &server)
{
    if (server.hintStonesRemaining() == 0) return false;

    const int numPlayers = handKnowledge_.size();
    Hint bestHint;
    for (int i = 1; i < ((numPlayers > 3) ? 3 : numPlayers); ++i) {
    //for (int i = 1; i < numPlayers; ++i) {
        const int partner = (me_ + i) % numPlayers;
	if (clueWaiting_[partner] || handLocked_[partner]) continue;
	// TODO: if only 1 hint left, maybe deemphasize hinting player with discardable
        Hint candidate = bestHintForPlayer(server, partner);
	if (candidate.fitness >= 0) {
	    bool hasPlayable = false;
	    for (int c=0; c < handKnowledge_[partner].size(); ++c) {
		const CardKnowledge &knol = handKnowledge_[partner][c];
		if (knol.playable() == YES) {
		    hasPlayable = true;
		    break;
		}
	    }
	    if (hasPlayable) {
		//candidate.fitness -= 20;
	    }
	    if (!hasPlayable) {
		// this player does not already have a playable card so cluing is worth more
		candidate.fitness += (numPlayers - i)*3;
	    }
	}
        if (candidate.fitness > bestHint.fitness) {
            bestHint = candidate;
        }
    }

    if (bestHint.fitness <= 0) return false;

    /* Give the hint. */
    bestHint.give(server);
    return true;
}

bool AwwBot::maybeGiveSuperHint(Server &server)
{
    if (server.hintStonesRemaining() == 0) return false;

    const int numPlayers = handKnowledge_.size();
    if (server.cardsRemainingInDeck() > 1) return false;

    const int partner = (me_ + 1) % numPlayers;
    Hint bestHint = bestHintForPlayer(server, partner);
    if (bestHint.fitness < 0) return false;
    bool hasPlayable = false;
    for (int c=0; c < handKnowledge_[partner].size(); ++c) {
	const CardKnowledge &knol = handKnowledge_[partner][c];
	if (knol.playable() == YES) {
	    hasPlayable = true;
	    break;
	}
    }
    if (hasPlayable) return false;

    // Give the hint.
    bestHint.give(server);
    return true;
}

bool AwwBot::maybePlayMysteryCard(Server &server)
{
    if (!UseMulligans) return false;
    if (handLocked_[me_]) return false; 
    if (clueWaiting_[(me_+1) % server.numPlayers()]) return false; 

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

bool AwwBot::maybeDiscardOldCard(Server &server)
{
    const int best_index = nextDiscardIndex(server, me_);
    // before running this, we already checked for worthless cards and found none
    assert(best_index < 32);
    if ((best_index % 16) != 0) {
        server.pleaseDiscard(best_index % 8);
        return true;
    }
    /* I didn't find anything I was willing to discard. */
    return false;
}

int AwwBot::cardOnChop(const Hanabi::Server &server, int to) const
{
    // find out which card is "on the chop"  
    const int numCards = handKnowledge_[to].size();
    int best_fitness = 0;
    int best_index = -1;
    for (int i=0; i < numCards; ++i) {
        const CardKnowledge &knol = handKnowledge_[to][i];
        int fitness = 0;
        if (knol.playable() == YES) continue;  /* we won't chop a playable (normally) */
        if (knol.worthless() == YES) return i;	// worthless is always first choice to chop
        if (knol.valuable() == YES) continue;  /* we should never discard this card */

        if ((best_index == -1) && (!knol.clued())) {
	    // if we don't find a worthless card, this will hold the oldest unclued card
            best_index = i;
        }
    }
    return best_index;
}

int AwwBot::upcomingIssues(Server &server)
{

    // Here we will score possible upcoming issues. 
    // 0 means no issues.
    // 100
    const int numPlayers = handKnowledge_.size();

    return 0;
}

void AwwBot::pleaseMakeMove(Server &server)
{
    assert(server.whoAmI() == me_);
    assert(server.activePlayer() == me_);
    assert(UseMulligans || !server.mulligansUsed());

    if (server.cardsRemainingInDeck() > server.numPlayers()) {
	if (maybeFinesseNextPlayer(server, 1, 2)) return;
	if (maybeGiveValuableWarning(server, 1)) return;
	if (server.hintStonesRemaining() == 1) {
	    // there is only 1 hint left and LH1 does not need a save clue
	    // give save clue to LH2 if needed
	    if (server.numPlayers() > 2) {
		if (maybeGiveValuableWarning(server, 2)) return;
	    }
	}
	if (maybePlayLowestPlayableCard(server)) return;
	if (server.numPlayers() > 2) {
	    if (maybeGiveValuableWarning(server, 2)) return;
	}
	if (maybeGiveHelpfulHint(server)) return;
	if (maybePlayMysteryCard(server)) return;
    } else if (server.cardsRemainingInDeck() == 0) {
        if (maybePlayLowestPlayableCard(server)) return;
	if (maybeGiveSuperHint(server)) return;
        if (maybePlayMysteryCard(server)) return;
	if (maybeGiveHelpfulHint(server)) return;
    } else {
	if (maybeGiveValuableWarning(server, 1)) return;
	if (maybePlayLowestPlayableCard(server)) return;
	if (maybeGiveHelpfulHint(server)) return;
	if (maybePlayMysteryCard(server)) return;
    }

    /* We couldn't find a good hint to give, or else we're out of hint-stones.
     * Discard a card. However, discarding is not allowed when we have all
     * the hint stones, so in that case, just hint a player 5 */

    if (!server.discardingIsAllowed()) {
        const int numPlayers = server.numPlayers();
        //const int right_partner = (me_ + numPlayers - 1) % numPlayers;
        server.pleaseGiveValueHint((me_ + 1) % numPlayers, Value(5));
    } else {
        if (maybeDiscardWorthlessCard(server)) return;
        if (maybeDiscardOldCard(server)) return;

        /* In this unfortunate case, which still happens fairly often, I find
         * that my whole hand is composed of valuable cards, and I just have
         * to discard the one of them that will block our progress the least. */
        int best_index = 0;
        for (int i=0; i < myHandSize_; ++i) {
            //assert(handKnowledge_[me_][i].valuable() == YES);
            if (handKnowledge_[me_][i].value() > handKnowledge_[me_][best_index].value()) {
                best_index = i;
            }
        }
        server.pleaseDiscard(best_index);
    }
}

// $Log: AwwBot.cc,v $
// Revision 1.11  2015/06/22 20:04:55  jay
// some clean up, no logic changes
//
// Revision 1.10  2015/06/22 20:00:13  jay
// 22.982 23.5665 22.8745 21.495
// fixed some besthint stuff
// also removed cluing player 4/5 in 4/5 player games
//
// Revision 1.9  2015/06/22 16:38:23  jay
// 22.973 23.489 22.668 21.153
// First code with finesse (can clue LH2 to finesse LH1)
//
// Revision 1.8  2015/06/18 20:35:51  jay
// init prep for delayed clues
// 22.892 22.711 21.944 20.382
//
// Revision 1.6  2015/06/15 19:52:23  jay
// Over 1000 games, AwwBot scored an average of 22.718 points per game.
//   9.8 percent were perfect games.
//   Mulligans used: 0 (45.7%); 1 (37.1%); 2 (15.5%); 3 (1.7%).
//
// Revision 1.5  2015/06/15 19:01:39  jay
// Over 1000 games, AwwBot scored an average of 22.441 points per game.
//   4.5 percent were perfect games.
//   Mulligans used: 0 (40%); 1 (42.5%); 2 (15.8%); 3 (1.7%).
//
// Revision 1.4  2015/06/14 17:13:43  jay
// AwwBot scored 22 points in that first game.
// Over 10000 games, AwwBot scored an average of 22.3169 points per game.
//   3.68 percent were perfect games.
//   Mulligans used: 0 (36.45%); 1 (39.69%); 2 (23.86%); 3 (0%).
//
// Revision 1.3  2015/06/14 16:19:32  jay
// --seed 2131690149
// Over 10000 games, AwwBot scored an average of 22.2792 points per game.
//   3.21 percent were perfect games.
//   Mulligans used: 0 (35.92%); 1 (39.67%); 2 (18.63%); 3 (5.78%).
//
// Revision 1.2  2015/06/14 14:32:19  jay
// not quite working first version
//
// Revision 1.1  2015/06/13 20:07:03  jay
// Initial revision
//
// Revision 1.1  2015/06/12 13:45:23  jay
// Initial revision
//
//
