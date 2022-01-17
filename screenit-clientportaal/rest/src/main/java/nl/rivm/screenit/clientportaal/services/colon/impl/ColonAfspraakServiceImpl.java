package nl.rivm.screenit.clientportaal.services.colon.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.math.BigDecimal;
import java.util.Date;

import nl.rivm.screenit.clientportaal.mappers.colon.ColonVrijSlotZonderKamerMapper;
import nl.rivm.screenit.clientportaal.model.colon.ColonVrijSlotZonderKamerDto;
import nl.rivm.screenit.clientportaal.services.colon.ColonAfspraakService;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.colon.enums.RedenAfspraakAfzeggen;
import nl.rivm.screenit.model.colon.planning.AfspraakStatus;
import nl.rivm.screenit.model.colon.planning.VrijSlotZonderKamer;
import nl.rivm.screenit.service.colon.AfspraakService;
import nl.rivm.screenit.service.colon.PlanningService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.planning.model.Discipline;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class ColonAfspraakServiceImpl implements ColonAfspraakService
{

	@Autowired
	private AfspraakService afspraakService;

	@Autowired
	private ColonVrijSlotZonderKamerMapper colonVrijSlotZonderKamerMapper;

	@Autowired
	private PlanningService planningService;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public ColonIntakeAfspraak getHuidigeIntakeAfspraak(Client client)
	{
		ColonScreeningRonde laatsteColonScreeningRonde = client.getColonDossier().getLaatsteScreeningRonde();
		if (laatsteColonScreeningRonde == null)
		{
			return null;
		}

		return laatsteColonScreeningRonde.getLaatsteAfspraak();
	}

	@Override
	public ColonVrijSlotZonderKamerDto vanVrijSlotNaarColonVrijSlot(VrijSlotZonderKamer vrijSlot)
	{
		ColonVrijSlotZonderKamerDto vrijColonSlot = colonVrijSlotZonderKamerMapper.vrijSlotToColonVrijSlotZonderKamerDto(vrijSlot);

		ColoscopieCentrum intakelocatie = hibernateService.load(ColoscopieCentrum.class, vrijColonSlot.getIntakeLocatieId());
		vrijColonSlot.setAdres(intakelocatie.getEersteAdres().getAdres());
		vrijColonSlot.setPostcode(intakelocatie.getEersteAdres().getPostcode());
		vrijColonSlot.setZiekenhuis(intakelocatie.getNaam());
		return vrijColonSlot;
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public void intakeAfspraakAfzeggen(Client client, RedenAfspraakAfzeggen redenAfzeggen)
	{
		ColonIntakeAfspraak intakeAfspraak = client.getColonDossier().getLaatsteScreeningRonde().getLaatsteAfspraak();
		if (intakeAfspraak == null)
		{
			throw new IllegalStateException("Intakeafspraak onbekend");
		}
		else
		{
			intakeAfspraak.setRedenAfzeggen(redenAfzeggen);

			afspraakService.annuleerAfspraak(intakeAfspraak, client, AfspraakStatus.GEANNULEERD_CLIENT, false);
		}
	}

	@Override
	public ColonIntakeAfspraak initNieuweAfspraak(ColonIntakeAfspraak oudeAfspraak, VrijSlotZonderKamer gekozenVrijSlotZonderKamer)
	{

		if (gekozenVrijSlotZonderKamer.getIntakeLocatieId() != null && (gekozenVrijSlotZonderKamer.getStartTijd() != null))
		{
			Kamer beschikbareKamer = planningService.getBeschikbareKamer(gekozenVrijSlotZonderKamer.getStartTijd(),
				gekozenVrijSlotZonderKamer.getIntakeLocatieId());

			if (beschikbareKamer == null)
			{
				throw new IllegalStateException("Iemand anders heeft het gekozen tijdstip in de tussentijd gevuld.");
			}

			ColonIntakeAfspraak nieuweAfspraak = new ColonIntakeAfspraak();
			nieuweAfspraak.setLocation(beschikbareKamer);
			nieuweAfspraak.setActief(true);
			nieuweAfspraak.setBezwaar(false);
			nieuweAfspraak.setDatumLaatsteWijziging(new Date());
			nieuweAfspraak.setAfspraaknummer(System.currentTimeMillis());
			nieuweAfspraak.setStatus(AfspraakStatus.GEPLAND);

			if (gekozenVrijSlotZonderKamer.getAfstand() != null)
			{
				nieuweAfspraak.setAfstand(BigDecimal.valueOf(gekozenVrijSlotZonderKamer.getAfstand()));
			}
			else
			{
				nieuweAfspraak.setAfstand(BigDecimal.valueOf(45));
			}
			nieuweAfspraak.addDiscipline(hibernateService.loadAll(Discipline.class).get(0));

			nieuweAfspraak.setColonScreeningRonde(oudeAfspraak.getColonScreeningRonde());
			nieuweAfspraak.setClient(oudeAfspraak.getClient());
			nieuweAfspraak.setDefinition(oudeAfspraak.getDefinition());
			nieuweAfspraak.setStartTime(gekozenVrijSlotZonderKamer.getStartTijd());
			nieuweAfspraak.setEndTime(gekozenVrijSlotZonderKamer.getEindTijd());

			return nieuweAfspraak;
		}
		return null;
	}
}
