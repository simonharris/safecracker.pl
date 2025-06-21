import { createRouter, createWebHistory } from 'vue-router'
import SolveView from '../views/SolveView.vue'
import AboutView from '../views/AboutView.vue'

const routes = [
  {
    path: '/',
    name: 'home',
    component: SolveView
  },
  {
    path: '/examples',
    name: 'examples',
    component: SolveView
  },
  {
    path: '/about',
    name: 'about',
    component: AboutView
  }
]

const router = createRouter({
  history: createWebHistory(process.env.BASE_URL),
  routes
})

export default router
