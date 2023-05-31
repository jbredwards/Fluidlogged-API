# Fluidlogged API

##### A mod that adds basic fluidlogging to 1.12, and serves a base or optional api for other mod devs to use in their own projects.

---

### Info For Mod Devs

##### Add the following to your `build.gradle` to add this mod's files to your workspace:

```groovy
dependencies {
    deobfCompile 'com.github.jbredwards:fluidlogged-api:cecb9f6c34'
}

repositories {
    maven { url 'https://jitpack.io' }
}
```
##### Make sure your project isn't using ForgeGradle 3+, it doesn't apply access transformers from dependencies, so your mod must use either ForgeGradle 2.3 or [FancyGradle](https://gitlab.com/gofancy/fancygradle/-/wikis/home).

##### Any mods using versions 1.8.0+ as a dependancy must make sure they're using stable_39 mappings for this mod to work properly in a deobfuscated enviornment!

##### View the [wiki](https://github.com/jbredwards/Fluidlogged-API/wiki) for more info. (currently wip)
