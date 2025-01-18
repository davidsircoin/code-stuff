public class Monster extends Character {
    private int giveExp;

    public Monster(String name, int hp, int damage, int giveExp) {
        super(name, hp, damage);
        this.giveExp = giveExp;
    }

    public int getGiveExp() {
        return giveExp;
    }
}


