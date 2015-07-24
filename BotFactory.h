
#ifndef H_BOT_FACTORY
#define H_BOT_FACTORY

#include "Hanabi.h"

template<class SpecificBot>
struct BotFactory : public Hanabi::BotFactory
{
    virtual Hanabi::Bot *create(int index, int numPlayers, int handSize, Hanabi::GameMode mode) const { return new SpecificBot(index, numPlayers, handSize, mode); }
    virtual void destroy(Hanabi::Bot *bot) const { delete bot; }
};

#endif /* H_BOT_FACTORY */
