<script setup>
import solverService from '@/services/SolverService.js';
</script>

<template>

  <h1>Solve from Examples</h1>

  <ExampleSelectForm @form-submitted="handleFormSubmission" />
  <!-- image-preview / -->
  <SolutionOutput :solverservice="service" />

</template>

<script>
import ExampleSelectForm from '@/components/ExampleSelectForm.vue'
import SolutionOutput from '@/components/SolutionOutput.vue'

export default {
  name: 'ExampleSolveView',
  components: {
    ExampleSelectForm,
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
    handleFormSubmission(puzzleid) {
      //alert('Form submitted');
      //this.solverservice = new SolverService();
      this.service.solveExample(puzzleid);
    }
  }
}
</script>


<style scoped>

</style>
