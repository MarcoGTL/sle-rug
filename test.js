var vm = new Vue({
    el: '#vm',
    data: {
        hasBoughtHouse: false,
        food: '',
        why: '',
        houses2010: 0,
        houses2011: 0,
        rich: false,
        buyingPrice: 0

    },
    methods: {
        housesTotal: function() {
            return eval(this.houses2010) + eval(this.houses2011);
        }
    }
})