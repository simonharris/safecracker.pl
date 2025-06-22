<script setup>
import solverService from '@/services/SolverService.js';
import FileUploadForm from '@/components/FileUploadForm.vue'
import SolutionOutput from '@/components/SolutionOutput.vue'
</script>

<template>

  <h1>Solve from Upload</h1>

  <p>Take a reasonably clear photo of the printed Safe Cracker puzzle, and upload it here. We'll see what we can do!</p>

  <FileUploadForm :solverservice="service" />
  <!-- image-preview / -->
  <SolutionOutput :solverservice="service" />

</template>


<script>

export default {
  name: 'UploadSolveView',
  components: {
    FileUploadForm,
    SolutionOutput,
  },
  data() {
    return {
      service: solverService,
      messages: [],
    }
  },
  setup() {
    // ...
  },
  mounted() {
    // ...
  },
  computed: {
    // ...
  },
  methods: {
    // ...
  }
}
</script>


<style scoped>

</style>

