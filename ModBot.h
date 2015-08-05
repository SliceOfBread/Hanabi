
#include "Hanabi.h"

class ModBot;

enum trivalue {
    NO, MAYBE, YES
};

enum issues {
    NONE,
    NEXTCHOP,
    NEXTTWOCHOP,
    AFTERNEXTCHOP
};
    

struct CardKnowledge {
    CardKnowledge();

    bool mustBe(Hanabi::Color color) const;
    bool mustBe(Hanabi::Value value) const;
    bool cannotBe(Hanabi::Card card) const;
    bool cannotBe(Hanabi::Color color) const;
    bool cannotBe(Hanabi::Value value) const;
    int color() const;
    int value() const;
    bool clued() const;

    void setMode(Hanabi::GameMode mode);
    void setClued(bool gotClued);
    int setMustBeMultiOr(Hanabi::Color color);
    int setMustBe(Hanabi::Color color);
    int setMustBe(Hanabi::Value value);
    int setCannotBeMulti();
    int setCannotBe(Hanabi::Color color);
    int setCannotBe(Hanabi::Value value);
    void setIsPlayable(const Hanabi::Server &server, bool knownPlayable);
    void setIsValuable(const ModBot &bot, const Hanabi::Server &server, bool knownValuable);
    void setIsWorthless(const ModBot &bot, const Hanabi::Server &server, bool knownWorthless);
    void update(const Hanabi::Server &server, const ModBot &bot, bool useMyEyesight);

    trivalue playable() const { return playable_; }
    trivalue valuable() const { return valuable_; }
    trivalue worthless() const { return worthless_; }

private:
    bool cantBe_[Hanabi::NUMCOLORS][5+1];
    Hanabi::GameMode mode_;
    int color_;
    int value_;
    trivalue playable_;
    trivalue valuable_;
    trivalue worthless_;
    bool clued_;
};

struct Hint {
    int fitness;
    int to;
    int color;
    int value;

    Hint();
    void give(Hanabi::Server &);
};

class ModBot : public Hanabi::Bot {

    friend class CardKnowledge;

    int me_;
    Hanabi::GameMode mode_;

    int myHandSize_;  /* purely for convenience */

    /* What does each player know about his own hand? */
    std::vector<std::vector<CardKnowledge> > handKnowledge_;
    // if player had (at time of clue) a playable card playClued=true, cluedindex holds position
    bool playClued_[5];
    // if player was ever clued a discard (and still has card), discardClued=true, cluedindex holds position
    bool discardClued_[5];
    int cluedIndex_[5];
    bool ignoreCluesIfDiscardValuable_;
    int maxEval_;

    bool priorPlayerDiscarded_;
    int priorDiscardColor_;
    int priorDiscardValue_;
    /* What cards have been played so far? */
    int playedCount_[Hanabi::NUMCOLORS][5+1];
    /* What cards in players' hands are definitely identified?
     * This table is recomputed every turn. */
    int locatedCount_[Hanabi::NUMCOLORS][5+1];
    /* What cards in players' hands are visible to me in particular?
     * This table is recomputed every turn. */
    int eyesightCount_[Hanabi::NUMCOLORS][5+1];
    /* What is the lowest-value card currently playable?
     * This value is recomputed every turn. */
    int lowestPlayableValue_;

    bool isPlayable(const Hanabi::Server &server, Hanabi::Card card) const;
    bool isValuable(const Hanabi::Server &server, Hanabi::Card card) const;
    bool isWorthless(const Hanabi::Server &server, Hanabi::Card card) const;
    bool couldBePlayableWithValue(const Hanabi::Server &server, const CardKnowledge &knol, int value) const;
    bool couldBeValuableWithValue(const Hanabi::Server &server, const CardKnowledge &knol, int value) const;

    void updateEyesightCount(const Hanabi::Server &server);
    bool updateLocatedCount(const Hanabi::Server &server);
    void invalidateKnol(int player_index, int card_index);
    void seePublicCard(const Hanabi::Card &played_card);

    void noValuableWarningWasGiven(const Hanabi::Server &server, int from);

    int handEval(const Hanabi::Server &server, int partner) const;
    int discardPriorityHandEval(const Hanabi::Server &server, int partner, bool excludePlay) const;
    int evaluateAllHands(const Hanabi::Server &server, int skipMe, int skipAlso, int *eval) const;

    int bestCardToPlay(Hanabi::Server &server);
    bool maybeDontDoubleDiscard(Hanabi::Server &server);
    bool maybePlayLowestPlayableCard(Hanabi::Server &server);
    bool maybeOverClueNextPlayer(Hanabi::Server &server);
    bool maybeGiveHelpfulHint(Hanabi::Server &server, bool forceClue);
    bool maybeGiveValuableWarning(Hanabi::Server &server);
    bool maybePlayMysteryCard(Hanabi::Server &server);

    void observeEval(const Hanabi::Server &server, int from, int evalInput);

  public:
    ModBot(int index, int numPlayers, int handSize, Hanabi::GameMode mode);
    virtual void pleaseObserveBeforeMove(const Hanabi::Server &);
    virtual void pleaseMakeMove(Hanabi::Server &);
      virtual void pleaseObserveBeforeDiscard(const Hanabi::Server &, int from, int card_index);
      virtual void pleaseObserveBeforePlay(const Hanabi::Server &, int from, int card_index);
      virtual void pleaseObserveColorHint(const Hanabi::Server &, int from, int to, Hanabi::Color color, const std::vector<int> &card_indices);
      virtual void pleaseObserveValueHint(const Hanabi::Server &, int from, int to, Hanabi::Value value, const std::vector<int> &card_indices);
    virtual void pleaseObserveAfterMove(const Hanabi::Server &);
    virtual bool botCanPlay(Hanabi::GameMode mode);
};

// $Log: ModBot.h,v $
//
//
