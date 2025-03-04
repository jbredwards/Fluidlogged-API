# Fluidlogged API

##### Fluidlogged API is a library mod that adds highly customizable fluidlogging to 1.12.2! Fluidlogged API also heavily improves fluid collision, and fixes various vanilla and modded fluid-related issues.

##### This repository has two active branches: [1.12.2-Indev](https://github.com/jbredwards/Fluidlogged-API/tree/1.12.2-Indev) and [1.12.2-Latest](https://github.com/jbredwards/Fluidlogged-API/tree/1.12.2-Latest). If you want to contribute to Fluidlogged API, any pull requests should be for the 1.12.2-Indev branch. 1.12.2-Latest reflects the code in the current version of Fluidlogged API, so anyone wanting to use Fluidlogged API as an optional or required dependency should use that branch.

---

### Fluidloggable Blocks

##### Any blocks can be added through Fluidlogged API's whitelist, however it does make certain types of blocks fluidloggable by default. These blocks are: Anvils, Banners, Barriers, Beacons, Brewing Stands, Buttons, Comparators, Chests, Daylight Detectors, Doors, Dragon Eggs, Enchantment Tables, End Portal Frames, End Rods, Ender Chests, Fences, Fence Gates, Glass Panes, Half Slabs, Hoppers, Iron Bars, Leaves, Levers, Mob Heads, Mob Spawners, Pressure Plates, Pistons *while extended*, Rails, Repeaters, Redstone Torches, Redstone Wire, Trapdoors, Tripwire Hooks, Tripwire String, Signs, and Walls. Any properly coded modded variants of these blocks are supported by default. Mods may also add fluidloggable blocks from their end. To see the full list of fluidloggable blocks, run the `/fluidlogged_api print` command in-game to create text files containing all active fluidlogging information.

##### Along with this, Fluidlogged API will download and apply [community configs](https://github.com/jbredwards/Fluidlogged-API-Configs)! This is to minimize the amount of work that players must do to have complete fluidlogging across mods, but it can be fully disabled in `fluidlogged_api/general.cfg` if desired.

---

### Info For Mod Devs

##### Add the following to your `build.gradle` to add this mod's files to your workspace:

```groovy
dependencies {
    deobfCompile 'com.github.jbredwards:fluidlogged-api:1.12.2-Latest-SNAPSHOT'
}

repositories {
    maven { url 'https://jitpack.io' }
}
```
##### Make sure your project isn't using ForgeGradle 3+, as it doesn't properly apply access transformers from dependencies. Your mod must use either ForgeGradle 2.3, [FancyGradle](https://gitlab.com/gofancy/fancygradle/-/wikis/home), or [RetroFuturaGradle](https://github.com/GTNewHorizons/RetroFuturaGradle). For this mod to work properly in a deobfuscated environment, make sure that you're using the `stable_39` forge mappings!

##### View the [wiki](https://github.com/jbredwards/Fluidlogged-API/wiki) for more info. (currently wip)
