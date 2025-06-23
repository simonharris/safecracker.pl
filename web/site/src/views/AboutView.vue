<template>
  <div class="about">
    <h1>About This Project</h1>

    <p>This project aims to automate the solving of the "Safe Cracker" numeric logic puzzle
      published in the Sunday Telegraph, based only on a photograph of the printed puzzle.</p>
      <p> Don't judge me: I only buy the objectionable rag for the puzzles.</p>

    <p>The project is open-source and the Prolog, Python and JavaScript code is all
      on Github as <a href="https://github.com/simonharris/safecracker.pl">safecracker.pl</a>.</p>

    <h2>The Puzzle</h2>

    <p>Each week the premise is the same:</p>

    <blockquote class="bg-light p-2">
      Four different digits between 1-9 [inclusive] are required to open a safe. From the
      clues below, can you work out the correct four-digit combination?
    </blockquote>

    <p>We are then provided with five more constraints, some simple, some more involved. For example:</p>

    <blockquote class="bg-light p-2">
      The first digit is greater than 5<br>
      Exactly two digits are prime.<br>
      Either the second or the third is odd, but not both.
    </blockquote>

    <p>With a pen and paper and a bit of persistence, we can whittle the pool of
      candidate solutions down from 3,024 to the one true combination.</p>

      <p>Or, given far
      too much time on our hands, we can program all the fun out of it for good.</p>

    <h2>How it works</h2>

      <p>The application has three subsystems:</p>

      <ul>
        <li>An NLP and logical reasoning engine written in Prolog</li>
        <li>A back-end system built in Python, which performs the OCR, interfaces with Prolog, and exposes an HTTP API</li>
        <li>A responsive front-end JavaScript web app</li>
      </ul>

    <p>I deliberately tried to use as many technologies and libraries that I hadn't
      extensively worked with before as possible, so the whole thing was quite a
      learning experience.</p>

    <h3>Prolog engine</h3>

      <p>This was probably the most challenging bit. I was lucky enough to spend a
        year in the <a href="https://www.ed.ac.uk/ai">Artificial Intelligence labs</a>
        at Edinburgh University as an undergraduate. If you were in AI at Edinburgh in the 90s,
        you were learning Prolog. 30 years later it was all a bit rusty!</p>

      <p>The system centres around a Natural Language Processing (NLP) engine based on a Definite Clause Grammar (DCG), thereby taking
         advantage of the fact the clues are presented in a relatively consistent format from week to week.</p>
         <p>
        The English text is parsed into <a href="https://www.swi-prolog.org/man/clpfd.html">CLP(FD)</a>
         constraints which are sequentially applied to the
        candidate solution pool, hopefully converging on the correct solution
        (as reinforced by the <a href="https://www.swi-prolog.org/pldoc/doc/_SWI_/library/ext/plunit/plunit.pl">PLUnit</a> test suite).</p>


    <h3>API</h3>

    <p>The back-end API is built in Python using the incredibly simple-yet-powerful
      <a href="https://flask.palletsprojects.com/en/stable/">Flask</a> framework.
      This takes care of one or two HTTP+JSON calls (of the type which are often
      erroneously described as REST), but does most of its work via streaming/socket-type
      functionality, sending real-time progress updates back to the browser.
      This is almost trivially easy in Flask.</p>

    <p>This is also where the Optical Character Recognition (OCR) happens, extracting
      the clue text from the uploaded cameraphone snap. This is done using the open-source
      <a href="https://github.com/tesseract-ocr/tesseract">Tesseract</a> OCR libraries,
      and the <a href="https://pypi.org/project/pytesseract/">pytesseract</a> wrapper for Python.</p>

      <p>Communication between Python and Prolog is achieved using <a href="https://pyswip.org/">PySwip</a>.</p>


    <h3>Web application</h3>

    <p>The front-end web app is what you're looking at now, and is built in JavaScript using the
       <a href="https://vuejs.org/">Vue.js</a> framework and
      the ubiquitous <a href="https://getbootstrap.com/">Bootstrap</a> toolkit.
      I hope it's responsive and mobile friendly.</p>

  </div>

</template>


