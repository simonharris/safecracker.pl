import { createRouter, createWebHistory } from 'vue-router'
import UploadSolveView from '../views/UploadSolveView.vue'
import ExampleSolveView from '../views/ExampleSolveView.vue'
import AboutView from '../views/AboutView.vue'

const routes = [
  {
    path: '/',
    name: 'home',
    component: UploadSolveView
  },
  {
    path: '/examples',
    name: 'examples',
    component: ExampleSolveView
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
