<script setup>
import solverService from '@/services/SolverService.js';
import FileUploadForm from '@/components/FileUploadForm.vue'
import SolutionOutput from '@/components/SolutionOutput.vue'
</script>

<template>

  <h1>Solve from Upload</h1>

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

