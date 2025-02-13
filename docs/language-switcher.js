document.addEventListener('DOMContentLoaded', function() {
    // Cria o container do seletor de idioma
    const languageSwitcher = document.createElement('div');
    languageSwitcher.className = 'language-switcher';

    // Cria o dropdown (select)
    const select = document.createElement('select');
    select.id = 'language-select';

    // Adiciona as opções de idioma
    const options = [
        { value: '/pt/index.qmd', text: 'Português' },
        { value: '/en/index.qmd', text: 'English' }
    ];

    options.forEach(option => {
        const opt = document.createElement('option');
        opt.value = option.value;
        opt.textContent = option.text;
        select.appendChild(opt);
    });

    // Adiciona o evento de mudança de idioma
    select.addEventListener('change', function() {
        window.location.href = this.value;
    });

    // Adiciona o dropdown ao container
    languageSwitcher.appendChild(select);

    // Adiciona o container à barra de navegação
    const navbar = document.querySelector('.navbar-collapse'); // Ajuste o seletor conforme necessário
    if (navbar) {
        navbar.appendChild(languageSwitcher);
    }
});