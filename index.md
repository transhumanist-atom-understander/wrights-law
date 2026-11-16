In 2024 we were posting about the astonishing progress in solar power.
People were telling me, first of all, that fossil fuels were obsolete
for electricity generation. And second, that for carbon-free electricity
generation, nuclear couldn’t compete with solar.

I had been converted to a solar believer by the growth of battery
storage, but I was skeptical of the visions of Solartopia, where we run
on solar all day and batteries all night. I think my skepticism has aged
well, as data centers install natural gas turbines and hyperscalers work
to restart nuclear plants. But the basis of my skepticism then was that
[batteries were being installed for a niche use, and that their price
would have to drop substantially to escape that
niche](https://www.lesswrong.com/posts/mnaEgW9JgiochnES2/2024-was-the-year-of-the-big-battery-and-what-that-means-for).

Of course, the astonishing price drops in both solar and batteries are
what made us all into solar believers. The hot graph we were all
reposting was a “learning curve”, showing how price decreases as a
function of cumulative production: how many watts of solar cells have
ever been produced, or kWh of lithium-ion batteries. [When such a
“curve” is a straight line on a log-log plot, that’s an instance of
Wright’s law](https://ourworldindata.org/learning-curve).

What irritated me about the discourse, though, was the prevalent idea
that if you believed in extrapolating these lines, then you believed
Solartopia was nigh. For some record that this idea was in the air, see
Scott Alexander’s [Notes from a Progress Studies
Conference](https://www.astralcodexten.com/p/notes-from-the-progress-studies-conference).

The problem with this is that the learning curves are not actually that
steep. Even taking for granted that we can extrapolate the learning
curve with a Wright’s law-style fit, what these graphs really show is
the massive scale of the effort required for each price decrease.

## Solar learning curve

Let’s take another look at the Wright’s law learning curve for solar.
Using [OWID
data](https://ourworldindata.org/grapher/solar-pv-prices-vs-cumulative-capacity),
plotting the price of solar modules as a function of cumulative
capacity:

<img src="index_files/figure-commonmark/solar-wrights-law-1.png"
data-fig-alt="Log-log plot of solar module cost against cumulative capacity, with a fitted straight line sloping downwards. Orders of magnitude are the same size on both axes." />

Of course the most impressive thing about this plot is the massive
reduction in price, across multiple orders of magnitude. But notice
there are more orders of magnitude on the x-axis than on the y-axis.
I’ve made sure that orders of magnitude are the same size on both axes,
so we can get a visual sense of the relationship.

The slope of the line fit is about -0.4. This means a 10x reduction in
price requires a scale-up of about 300x in cumulative capacity. That’s
how hard people fought for every order of magnitude in price reduction.

## Reconsidering solar vs nuclear

Solar is already so cheap that we would be using it for everything if we
could easily store and transmit it. What was new in 2024 was the boom in
battery storage, driven by [a similar learning curve for battery
cells](https://ourworldindata.org/grapher/battery-price-vs-cumulative-production).
Eventually batteries will be cheap enough that it’s economical to
discharge them all night at current electricity prices. But given what
these learning curves show, that will only happen once the industry has
been rebuilt many times over.

Let’s reconsider the comparison to nuclear, keeping in mind the massive
investment and engineering effort that such a battery scale-up involves.
It’s not easy to build a nuclear plant. [Europe’s newest
reactor](https://en.wikipedia.org/wiki/Flamanville_Nuclear_Power_Plant#Unit_3)
was a particular embarrassment to nuclear, running far behind schedule
and over budget. But in Europe they can’t seem to build battery
factories either; I’m thinking of [how Northvolt couldn’t get production
going](https://www.ft.com/content/09938004-21b9-4750-8fa2-9ed15c566d4e).
In China, where they can build solar and battery factories, they can
also build nuclear plants, [adding nuclear generation every year, though
solar is growing far
faster](https://ourworldindata.org/grapher/electricity-production-by-source?country=~CHN).
[China also has a small modular reactor
(SMR)](https://www.nucnet.org/news/hot-testing-begins-at-china-s-linglong-1-small-modular-reactor-10-3-2026),
and if they begin factory production of SMRs, we could see price
learning curves for nuclear too. The effort of starting the SMR industry
from nothing seems massive, but producing many times more battery cells
than have ever been produced is also a massive effort. Compare one
massive effort to another, and imagine both happening in China, and I
think it is not so clear which will be the better option for zero-carbon
all-night power.

I do think solar and batteries will be massively scaled up, and I do
think they will become much cheaper, more or less along the Wright’s law
lines. But if efforts of the same scale are made in nuclear, I think it
may be cost-competitive, even in this high-solar world.

## Conclusion

You do greater honor to the god of straight lines on graphs when you
look at the slope. When it’s a Wright’s law fit, the slope tells us
about the scale of production efforts that accompany reductions in
price. When the slope is not steep, such efforts are massive, so
consider the competition to be other efforts of the same scale.
