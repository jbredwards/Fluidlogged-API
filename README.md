# Fluidlogged API

##### A mod that adds basic fluidlogging to 1.12, and serves a base or optional api for other mod devs to use in their own projects.

---

### Info For Mod Devs

##### Add the following to your `build.gradle` to add this mod's files to your workspace:

```groovy
dependencies {
    provided 'com.github.jbredwards:fluidlogged-api:ccf9bba5ec'
    compile  'com.github.jbredwards:fluidlogged-api:ccf9bba5ec'
}

repositories {
    maven { url 'https://jitpack.io' }
}
```

##### Remember to have either the spongeforge mixin library (ver 0.7 or greater) in your workspace or a mod that supplies it to have this work properly

##### View the [wiki](https://github.com/jbredwards/Fluidlogged-API/wiki) for more info. (currently wip)
