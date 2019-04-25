# PiWorld

PiWorld is a multiplayer real time strategy game that is written in Haskell and is played over the internet. It is a hobby project written in order to better understand Haskell and web development.

## Concept

The game is inspired by other games such as *Tribal Wars* and *Doodle God* as well as some other games like *Cultures: The Viking Village*.

Each player starts with a village and 4 villagers. The villagers perform all work, and if they die it is a devastating loss for a player. The players all play on the same world, they can interact if they find each other. They can send messages to each other, trade and even fight.

While in most games the objective is to conquer all other villages, in this game the goal is rather to cooperate. This is because there will be outside forces that threaten the players. Also, not all villages will have access to the same type of resources, therefore villages that trade among themselves will be more advanced that those who do not.

In this game there is no set research tree with a canonical line of advancements. The player can discover new technologies by experimenting with the materials they have access to, similar to the browser game *Doodle God*. The players are encouraged to pursue their own technological path. One player can pursue a modern industrial path, another can pursue a steampunk city that runs on coal, while another can use windmills or animals to run their industry. We would like to give as many branching technology paths as possible.

## Villagers

Each villager needs food and rest each day. The player can assign jobs for each individual villager. These jobs can be jobs to gather raw materials such as woodcutter or hunter, more complex jobs that transform raw materials like baker or miller, or miscellanious jobs such as builder or explorer.

### Jobs

Currently villagers can have the following jobs:

- Gatherer jobs:
  - Hunter (gathers Food from Grass and Woods)
  - Woodcutter (gathers Wood from Woods)
  - Stone Gatherer (gathers Stone from Rocky Hills)

- Advanced jobs:
  - N/A

- Miscellanious jobs:
  - Explorer (discovers territory around the village)

Each gatherer can gather 10 resources per day as a base gather rate, which can be improved by using tools.

### Tools

Villagers can equip tools to be more efficient.  
TODO

### Needs

Each villager will eat 3 food per day, otherwise they will grow hungry. If they get hungry, they start to starve and they will die in 5 days if they have no food.

## Backlog (will be removed and moved to Projects trello)

:black_square_button: All villagers can have babies

:black_square_button: When a villager has a baby, that villager can not work for a given time

:black_square_button: The first 4 villagers have set names, but you can change the names of the babies

:black_square_button: There is a research minigame similar to doodle good, where you combine a number of resources to get a new resource (you loose resources by doing so)

:black_square_button: Towns usually have only 1 or 2 major resources nearby

:black_square_button: Towns need to trade between each other to gain technology and resources

:black_square_button: If a players gets conquered, if his villagers escape alive, they rally to a new place to create a new town

:black_square_button: If a player doesn't start a new town the villagers start to wonder about

:black_square_button: Villagers have skill points and other attributes

:black_square_button: Buildings can be used as shelter for villagers or as storage or work stations

