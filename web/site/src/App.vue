

<template>

<nav class="navbar navbar-expand-md bg-dark border-bottom border-body" data-bs-theme="dark">
  <div class="container-fluid">
    <router-link @click="navOpen = false" class="navbar-brand sitename" to="/">SafeCracker</router-link>
    <button  @click="navOpen = !navOpen" class="navbar-toggler" type="button" data-bs-toggle="collapse"
        data-bs-target="#navbarSupportedContent" aria-controls="navbarSupportedContent"
        aria-expanded="false" aria-label="Toggle navigation">
      <span class="navbar-toggler-icon"></span>
    </button>
    <div class="collapse navbar-collapse" :class="{ show: navOpen }" id="navbarSupportedContent">
      <ul class="navbar-nav me-auto mb-2 mb-md-0">
        <li class="nav-item">
          <router-link @click="navOpen = false" class="nav-link" to="/">Upload</router-link>
        </li>
        <li class="nav-item">
          <router-link @click="navOpen = false" class="nav-link" to="/examples">Examples</router-link>
        </li>
        <li class="nav-item">
          <router-link @click="navOpen = false" class="nav-link" to="/about">About</router-link>
        </li>
      </ul>
    </div>
  </div>
</nav>


<main role="main" class="container-fluid">
  <div class="container-lg mt-4 pb-3">
    <router-view v-slot="{ Component }">
      <keep-alive>
        <component :is="Component" />
      </keep-alive>
    </router-view>
  </div>
</main>


<footer class="footer mt-1 border-top">
  <div class="container-fluid bg-light" Xdata-bs-theme="dark">
    <div class="container-lg text-body-secondary">

      <div class="row">
      <div class="col-6 pt-3 pb-3">Made by <a href="https://simon.colchester.social/">Simon Harding</a>.</div>
      <div class="col-6 text-end pt-2">
          <a class="text-body-secondary" href="http://github.com/simonharris/safecracker.pl">
            <i class="bi bi-github fs-3 me-3"></i>
          </a>
        </div>
      </div>
    </div>

  </div>
</footer>


<!-- /div -->

</template>


<script>
export default {
  data() {
    return {
      navOpen: false
    }
  }
}
</script>

<style>

</style>
