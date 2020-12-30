
package nl.rivm.screenit.service.cervix.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.dao.cervix.CervixMonsterDao;
import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.verslag.cytologie.CervixCytologieCytologieUitslagBvoBmhk;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.service.cervix.CervixVerwerkVerslagService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS)
public class CervixVerwerkVerslagServiceImpl implements CervixVerwerkVerslagService
{

	private static final Logger LOG = LoggerFactory.getLogger(CervixVerwerkVerslagServiceImpl.class);

	@Autowired
	private ApplicationContext appContext;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	@Autowired
	private CervixMonsterDao monsterDao;

	@Autowired
	private CervixMailService mailService;

	@Override
	public void verwerkInDossier(CervixCytologieVerslag cytologieVerslag)
	{

		CervixCytologieCytologieUitslagBvoBmhk cytologieUitslagBvoBmhk = cytologieVerslag.getVerslagContent().getCytologieUitslagBvoBmhk();
		String monsterIdentificatie = cytologieUitslagBvoBmhk.getMonsterBmhk().getMonsterIdentificatie();
		String papKlasseCode = cytologieUitslagBvoBmhk.getPapKlasse().getCode();
		CervixCytologieUitslag cytologieUitslag = CervixCytologieUitslag.valueOf(papKlasseCode.toUpperCase());

		monsterIdentificatie = monsterIdentificatie.replaceFirst("^0+(?!$)", ""); 
		CervixUitstrijkje uitstrijkje = monsterDao.getUitstrijkje(monsterIdentificatie);
		CervixScreeningRonde ontvangstRonde = uitstrijkje.getOntvangstScreeningRonde();

		if (!ontvangstRonde.getVerslagen().contains(cytologieVerslag))
		{
			cytologieVerslag.setCytologieUitslag(cytologieUitslag);
			cytologieVerslag.setUitstrijkje(uitstrijkje);

			cytologieVerslag.setScreeningRonde(ontvangstRonde);

			uitstrijkje.setCytologieVerslag(cytologieVerslag);
			uitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.BEOORDEELD_DOOR_CYTOLOGIE);
			uitstrijkje.setStatusDatum(dateSupplier.getDate());

			ontvangstRonde.getVerslagen().add(cytologieVerslag);
			if (cytologieUitslag != CervixCytologieUitslag.PAP0 && uitstrijkje.getBrief() == null)
			{
				if (ontvangstRonde.getInVervolgonderzoekDatum() == null)
				{
					if (ontvangstRonde.getUitstrijkjeCytologieUitslag() == null)
					{
						ontvangstRonde.setUitstrijkjeCytologieUitslag(uitstrijkje);
					}
				}
				else
				{
					if (ontvangstRonde.getUitstrijkjeVervolgonderzoekUitslag() == null)
					{
						ontvangstRonde.setUitstrijkjeVervolgonderzoekUitslag(uitstrijkje);
					}
				}
			}
		}
		else
		{
			cytologieVerslag.getHerzieningenOntvangen().add(dateSupplier.getDate());
		}
		hibernateService.saveOrUpdate(cytologieVerslag);
		hibernateService.saveOrUpdate(uitstrijkje);
		hibernateService.saveOrUpdate(ontvangstRonde);

		if (uitstrijkje.getBrief() != null && CervixOmissieType.WACHT_OP_CYTOLOGIE_UITSLAG.equals(uitstrijkje.getBrief().getOmissieType()))
		{
			mailService.sendOnbeoordeelbaarMaarTochOntvangstBeoordeling(cytologieVerslag.getUitstrijkje());
		}

	}

	@Override
	public void onAfterVerwerkVerslagContent(CervixCytologieVerslag cytologieVerslag)
	{
		cytologieVerslag.setDatumOnderzoek(cytologieVerslag.getVerslagContent().getVerrichting().getEindeVerrichting());
	}
}
