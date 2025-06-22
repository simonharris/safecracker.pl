

<template>



<nav class="navbar navbar-expand-md bg-dark border-bottom border-body" data-bs-theme="dark">
  <div class="container-fluid">
    <router-link class="navbar-brand sitename" to="/">SafeCracker</router-link>
    <button class="navbar-toggler" type="button" data-bs-toggle="collapse"
        data-bs-target="#navbarSupportedContent" aria-controls="navbarSupportedContent"
        aria-expanded="false" aria-label="Toggle navigation">
      <span class="navbar-toggler-icon"></span>
    </button>
    <div class="collapse navbar-collapse" id="navbarSupportedContent">
      <ul class="navbar-nav me-auto mb-2 mb-lg-0">
        <li class="nav-item">
          <router-link class="nav-link" to="/">Upload</router-link>
        </li>
        <li class="nav-item">
          <router-link class="nav-link" to="/examples">Examples</router-link>
        </li>
        <li class="nav-item">
          <router-link class="nav-link" to="/about">About</router-link>
        </li>
      </ul>
    </div>
  </div>
</nav>

<main>
<div class="container-fluid">
  <div class="container-lg mt-4">
    <router-view v-slot="{ Component }">
  <keep-alive>
    <component :is="Component" />
  </keep-alive>
</router-view>
  </div><!-- /container-lg -->
</div><!-- /container-fluid -->
</main>



<br>
<div class="container-fluid bg-dark border-top  fixed-bottom" data-bs-theme="dark">
<!-- div class="container-lg" -->

<footer class="d-flex flex-wrap justify-content-between align-items-center pt-2">
   <div class="col-md-4 d-flex align-items-center">
       <!-- a href="/" class="mb-3 me-2 mb-md-0 text-body-secondary text-decoration-none lh-1" aria-label="Bootstrap">
            <svg class="bi" width="30" height="24" aria-hidden="true">
                <use xlink:href="#bootstrap"></use>
            </svg>
        </a -->
        <span class="mb-2 text-body-secondary">Made by <a href="https://simon.colchester.social/">Simon Harding</a>.</span>
    </div>

    <ul class="nav col-md-4 justify-content-end list-unstyled d-flex">
        <!-- li class="ms-3">
            <a class="text-body-secondary" href="#" aria-label="Instagram">
                <svg class="bi" width="24" height="24" aria-hidden="true"><use xlink:href="#instagram"></use></svg>
            </a>
        </li -->
        <li class="ms-3 mb-2">
            <a class="text-body-secondary" href="http://github.com/simonharris/safecracker.pl" aria-label="Github">
                <i class="bi bi-github fs-3"></i>
            </a>
        </li>
    </ul>
</footer>

<!-- /div -->
</div>

</template>

<style>

</style>
