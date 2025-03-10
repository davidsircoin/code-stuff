import java.util.Random;

public class Game{

	public void fight(Hero hero, Monster monster) {
		System.out.println(hero.getName() +
		 " is going to fight " +
		  monster.getName() + "!");
         if (hero.getHP() <= 0) {
             System.out.println("But " + hero.getName() +
                     " was already dead :(");
             return;
         }

		while (hero.getHP() > 0 && monster.getHP() > 0){
			
			Random rand = new Random();
			int x = rand.nextInt(101);
			
			if (x+10* hero.getLevel() > 50) {
				monster.decHP(hero.getDamage());
				System.out.println(hero.getName() +
					" hits " + monster.getName() +
					" and does " + hero.getDamage() +
					" damage!");
			}	
			
			else {
				hero.decHP(monster.getDamage());
				System.out.print(monster.getName() +
					" hits " + hero.getName() +
					" and does " + monster.getDamage() +
					" damage!\n");
			}
		}

		if (monster.getHP() <= 0) {
			System.out.println(hero.getName() +
				" won the fight and got " + 
				monster.getGiveExp() + " exp!");
			hero.incExp(monster.getGiveExp());
		}
		else {
			System.out.println(monster.getName() +
				" won the fight and killed " +
				 hero.getName());
		}

	}
}
