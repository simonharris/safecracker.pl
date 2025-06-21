<script setup>
//import SolverService from '@/services/SolverService.js';
import FileUploadForm from '@/components/FileUploadForm.vue'
import SolutionOutput from '@/components/SolutionOutput.vue'
</script>

<template>

  <h1>Solve from Upload</h1>

  <FileUploadForm />
  <!-- image-preview / -->
  <SolutionOutput :solverservice="solverservice" />

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
      solverservice: Object(),
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
    // handleFormSubmission() {
    //   alert('Form submitted');
    //   //this.solverservice = new SolverService();
    //   //this.solverservice.solveExample(puzzleid);
    // }
  }
}
</script>


<style scoped>

</style>

