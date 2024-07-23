package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixMonster;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.CervixZas;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixHpvBeoordelingWaarde;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.CervixZasStatus;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.service.cervix.CervixBepaalVervolgService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;

public class CervixBepaalVervolgContext
{
	public CervixMonster huidigeMonster;

	public Date huidigeOntvangstdatum;

	public CervixMonsterType huidigMonsterType;

	public CervixUitstrijkje huidigUitstrijkje = null;

	public CervixUitstrijkjeStatus huidigUitstrijkjeStatus = null;

	public CervixLabformulier huidigLabformulier = null;

	public boolean huidigLabformulierAfgekeurd = false;

	public boolean huidigLabformulierGecontroleerdVoorCytologie = false;

	public CervixZas huidigeZas = null;

	public CervixZasStatus huidigeZasStatus = null;

	public CervixBrief huidigeMonsterBrief;

	public CervixScreeningRonde ontvangstRonde;

	public CervixMonster monsterHpvUitslag = null;

	public CervixHpvBeoordelingWaarde hpvUitslag = null;

	public Boolean hpvUitslagVanZasNogBriefVerstuurd = null;

	public CervixUitstrijkje uitstrijkjeCytologieUitslag = null;

	public CervixCytologieUitslag cytologieUitslag = null;

	public Date inVervolgonderzoekDatum = null;

	public CervixUitstrijkje uitstrijkjeVervolgonderzoekUitslag = null;

	public CervixCytologieUitslag vervolgonderzoekUitslag = null;

	public boolean huidigeOntvangstInVervolgonderzoek = false;

	public boolean isZasHoudbaar = false;

	public LocalDateTime nu;

	public LocalDate startDatumGenotypering;

	public CervixBaseMonsterService monsterService;

	public CervixBepaalVervolgService bepaalVervolgService;

	public int intervalControleUitstrijkje;

	public CervixBepaalVervolgContext(CervixMonster monster, boolean isZasHoudbaar, LocalDateTime nu, LocalDate startDatumAanleveringGenotypering,
		CervixBepaalVervolgService bepaalVervolgService, CervixBaseMonsterService monsterService, int intervalControleUitstrijkje)
	{
		this(monster, isZasHoudbaar, nu, startDatumAanleveringGenotypering, bepaalVervolgService, monsterService, intervalControleUitstrijkje, false);
	}

	public CervixBepaalVervolgContext(CervixMonster monster, boolean isZasHoudbaar, LocalDateTime nu, LocalDate startDatumAanleveringGenotypering,
		CervixBepaalVervolgService bepaalVervolgService, CervixBaseMonsterService monsterService, int intervalControleUitstrijkje, boolean digitaalLabformulier)
	{
		huidigeMonster = (CervixMonster) HibernateHelper.deproxy(monster);
		this.nu = nu;
		startDatumGenotypering = startDatumAanleveringGenotypering;
		this.bepaalVervolgService = bepaalVervolgService;
		this.monsterService = monsterService;

		huidigeOntvangstdatum = huidigeMonster.getOntvangstdatum();
		huidigeMonsterBrief = huidigeMonster.getBrief();

		if (huidigeMonster instanceof CervixUitstrijkje)
		{
			huidigMonsterType = CervixMonsterType.UITSTRIJKJE;
			huidigUitstrijkje = (CervixUitstrijkje) huidigeMonster;
			huidigUitstrijkjeStatus = huidigUitstrijkje.getUitstrijkjeStatus();
			if (digitaalLabformulier && huidigUitstrijkjeStatus == CervixUitstrijkjeStatus.NIET_ONTVANGEN)
			{
				huidigUitstrijkjeStatus = CervixUitstrijkjeStatus.ONTVANGEN;
			}
			huidigLabformulier = huidigUitstrijkje.getLabformulier();
			if (huidigLabformulier != null)
			{
				if (huidigLabformulier.getStatus() == CervixLabformulierStatus.AFGEKEURD)
				{
					huidigLabformulierAfgekeurd = true;
				}
				else if (huidigLabformulier.getStatus() == CervixLabformulierStatus.GECONTROLEERD_CYTOLOGIE)
				{
					huidigLabformulierGecontroleerdVoorCytologie = true;
				}
			}
			ontvangstRonde = huidigUitstrijkje.getOntvangstScreeningRonde();
		}
		else
		{
			huidigMonsterType = CervixMonsterType.ZAS;
			huidigeZas = (CervixZas) huidigeMonster;
			huidigeZasStatus = huidigeZas.getZasStatus();
			this.isZasHoudbaar = isZasHoudbaar;
			ontvangstRonde = huidigeZas.getOntvangstScreeningRonde();
		}

		if (ontvangstRonde != null)
		{
			monsterHpvUitslag = (CervixMonster) HibernateHelper.deproxy(ontvangstRonde.getMonsterHpvUitslag());
			if (monsterHpvUitslag != null)
			{
				hpvUitslag = monsterHpvUitslag.getLaatsteHpvBeoordeling().getHpvUitslag();
				hpvUitslagVanZasNogBriefVerstuurd = monsterHpvUitslag instanceof CervixZas && monsterHpvUitslag.getBrief() == null;
			}

			uitstrijkjeCytologieUitslag = ontvangstRonde.getUitstrijkjeCytologieUitslag();
			if (uitstrijkjeCytologieUitslag != null)
			{
				cytologieUitslag = this.uitstrijkjeCytologieUitslag.getCytologieVerslag().getCytologieUitslag();
			}

			inVervolgonderzoekDatum = ontvangstRonde.getInVervolgonderzoekDatum();
			if (inVervolgonderzoekDatum != null)
			{
				uitstrijkjeVervolgonderzoekUitslag = ontvangstRonde.getUitstrijkjeVervolgonderzoekUitslag();
				if (uitstrijkjeVervolgonderzoekUitslag != null)
				{
					vervolgonderzoekUitslag = uitstrijkjeVervolgonderzoekUitslag.getCytologieVerslag().getCytologieUitslag();
				}

				if (huidigeOntvangstdatum == null || inVervolgonderzoekDatum.before(huidigeOntvangstdatum))
				{
					huidigeOntvangstInVervolgonderzoek = true;
				}
			}
		}
		this.intervalControleUitstrijkje = intervalControleUitstrijkje;
	}
}
