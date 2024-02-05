
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.model.cervix.CervixCytologieVerslag;
import nl.rivm.screenit.model.cervix.enums.CervixCytologieUitslag;
import nl.rivm.screenit.model.cervix.enums.CervixOmissieType;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixBaseMonsterService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.service.cervix.CervixVerwerkVerslagService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.stereotype.Service;

@Slf4j
@Service
@AllArgsConstructor
public class CervixVerwerkVerslagServiceImpl implements CervixVerwerkVerslagService
{
	private final HibernateService hibernateService;

	private final ICurrentDateSupplier dateSupplier;

	private final CervixBaseMonsterService monsterService;

	private final CervixMailService mailService;

	@Override
	public void verwerkInDossier(CervixCytologieVerslag cytologieVerslag)
	{

		var cytologieUitslagBvoBmhk = cytologieVerslag.getVerslagContent().getCytologieUitslagBvoBmhk();
		var monsterIdentificatie = cytologieUitslagBvoBmhk.getMonsterBmhk().getMonsterIdentificatie();
		var papKlasseCode = cytologieUitslagBvoBmhk.getPapKlasse().getCode();
		var cytologieUitslag = CervixCytologieUitslag.valueOf(papKlasseCode.toUpperCase());

		monsterIdentificatie = monsterIdentificatie.replaceFirst("^0+(?!$)", ""); 
		var uitstrijkje = monsterService.getUitstrijkje(monsterIdentificatie).orElseThrow(() ->
			new IllegalStateException(
				String.format("Dossier kan niet worden verwerkt, het uitstrijkje voor verslag met id %d kan niet worden gevonden", cytologieVerslag.getId())));
		var ontvangstRonde = uitstrijkje.getOntvangstScreeningRonde();

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
