package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;

import nl.rivm.screenit.mamma.se.dto.actions.OnderzoekStartenDto;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.OnderzoekStartenService;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.service.BerichtToBatchService;
import nl.rivm.screenit.service.mamma.MammaBaseKansberekeningService;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class OnderzoekStartenServiceImpl implements OnderzoekStartenService
{
	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BerichtToBatchService hl7BerichtenToBatchService;

	@Autowired
	private MammaBaseKansberekeningService baseKansberekeningService;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Override
	public void starten(OnderzoekStartenDto action, MammaScreeningsEenheid screeningsEenheid, LocalDateTime transactieDatumTijd, InstellingGebruiker gebruiker)
	{
		final MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), gebruiker);
		zetAfspraakInOnderzoek(afspraak);
		maakOnderzoek(afspraak, screeningsEenheid, action.getAmputatie(), transactieDatumTijd);
		hl7BerichtenToBatchService.queueMammaHL7v24BerichtUitgaand(afspraak.getUitnodiging().getScreeningRonde().getDossier().getClient(), MammaHL7v24ORMBerichtStatus.STARTED);
	}

	private void zetAfspraakInOnderzoek(MammaAfspraak afspraak)
	{
		afspraak.setStatus(MammaAfspraakStatus.ONDERZOEK);
		hibernateService.saveOrUpdate(afspraak);
	}

	private void maakOnderzoek(MammaAfspraak afspraak, MammaScreeningsEenheid screeningsEenheid, MammaAmputatie amputatie, LocalDateTime transactieDatumTijd)
	{
		if (afspraak.getOnderzoek() == null)
		{
			MammaOnderzoek onderzoek = new MammaOnderzoek();
			onderzoek.setAfspraak(afspraak);
			onderzoek.setCreatieDatum(DateUtil.toUtilDate(transactieDatumTijd));
			onderzoek.setScreeningsEenheid(screeningsEenheid);
			onderzoek.setStatus(MammaOnderzoekStatus.ACTIEF);
			onderzoek.setOperatieRechts(Boolean.FALSE);
			onderzoek.setOperatieLinks(Boolean.FALSE);
			onderzoek.setAmputatie(amputatie);

			afspraak.setOnderzoek(onderzoek);

			MammaScreeningRonde screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
			screeningRonde.setLaatsteOnderzoek(onderzoek);
			baseKansberekeningService.screeningRondeSampleHerzien(screeningRonde);

			hibernateService.saveOrUpdateAll(onderzoek, afspraak, screeningRonde);
			baseKansberekeningService.dossierEventHerzien(screeningRonde.getDossier());
		}
	}
}
