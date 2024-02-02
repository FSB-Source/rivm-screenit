package nl.rivm.screenit.mamma.se.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-se-rest-bk
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

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.mamma.se.dto.actions.MammografieOpslaanDto;
import nl.rivm.screenit.mamma.se.dto.onderzoek.AnnotatieAfbeeldingSeDto;
import nl.rivm.screenit.mamma.se.service.MammaAfspraakService;
import nl.rivm.screenit.mamma.se.service.MammografieAanmakenService;
import nl.rivm.screenit.mamma.se.service.dtomapper.AfbeeldingDtoMapper;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaAnnotatieIcoon;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaMammografie;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.service.mamma.MammaBaseAnnotatieAfbeeldingService;
import nl.rivm.screenit.service.mamma.MammaBaseFactory;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.REQUIRED)
public class MammografieAanmakenServiceImpl implements MammografieAanmakenService
{

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private MammaBaseAnnotatieAfbeeldingService baseAnnotatieAfbeeldingService;

	@Autowired
	private MammaBaseFactory baseFactory;

	@Autowired
	private MammaAfspraakService afspraakService;

	@Override
	public void opslaanEnStatusovergang(MammografieOpslaanDto action, InstellingGebruiker instellingGebruiker, LocalDateTime transactieDatumTijd)
	{
		final MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		MammaOnderzoek onderzoek = getOnderzoek(action, afspraak);
		MammaScreeningRonde screeningRonde = afspraak.getUitnodiging().getScreeningRonde();
		MammaDossier dossier = screeningRonde.getDossier();
		MammaMammografie mammografie = getOfMaakMammografie(onderzoek);

		verwerkVisueleInspectieAfbeelding(onderzoek, action);
		mammografieVerwerken(mammografie, instellingGebruiker, dossier, transactieDatumTijd);

		hibernateService.saveOrUpdateAll(dossier, onderzoek, mammografie);
	}

	@Override
	public void opslaan(MammografieOpslaanDto action, InstellingGebruiker instellingGebruiker)
	{
		final MammaAfspraak afspraak = afspraakService.getOfMaakLaatsteAfspraakVanVandaag(action.getAfspraakId(), instellingGebruiker);
		MammaOnderzoek onderzoek = getOnderzoek(action, afspraak);
		MammaMammografie mammografie = getOfMaakMammografie(onderzoek);

		verwerkVisueleInspectieAfbeelding(onderzoek, action);
		hibernateService.saveOrUpdateAll(onderzoek, mammografie);
	}

	private MammaMammografie getOfMaakMammografie(MammaOnderzoek onderzoek)
	{
		MammaMammografie mammografie = onderzoek.getMammografie();
		if (mammografie == null)
		{
			mammografie = baseFactory.maakMammografie(onderzoek, null, null);

			onderzoek.setMammografie(mammografie);
		}
		return mammografie;
	}

	private void mammografieVerwerken(MammaMammografie mammografie, InstellingGebruiker instellingGebruiker, MammaDossier dossier, LocalDateTime transactieDatumTijd)
	{
		mammografie.setAfgerondDoor(instellingGebruiker);
		mammografie.setAfgerondOp(DateUtil.toUtilDate(transactieDatumTijd));
	}

	private void verwerkVisueleInspectieAfbeelding(MammaOnderzoek onderzoek, MammografieOpslaanDto action)
	{
		AfbeeldingDtoMapper afbeeldingDtoMapper = new AfbeeldingDtoMapper();
		MammaMammografie mammografie = onderzoek.getMammografie();
		MammaAnnotatieAfbeelding visueleInspectieAfbeelding = mammografie.getVisueleInspectieAfbeelding();
		if (action.getMammografie().getVisueleInspectieAfbeelding() != null
			&& action.getMammografie().getVisueleInspectieAfbeelding().getIconen() != null && action.getMammografie().getVisueleInspectieAfbeelding().getIconen().size() > 0)
		{
			if (visueleInspectieAfbeelding == null)
			{
				visueleInspectieAfbeelding = new MammaAnnotatieAfbeelding();
				mammografie.setVisueleInspectieAfbeelding(visueleInspectieAfbeelding);
				hibernateService.saveOrUpdate(visueleInspectieAfbeelding);
			}

			AnnotatieAfbeeldingSeDto afbeeldingDto = action.getMammografie().getVisueleInspectieAfbeelding();
			List<MammaAnnotatieIcoon> iconen = afbeeldingDto.getIconen().stream().map(afbeeldingDtoMapper::icoonDtoToAnnotatieIcoon).collect(Collectors.toList());
			baseAnnotatieAfbeeldingService.updateIconenInAfbeelding(iconen, visueleInspectieAfbeelding);
		}
		else
		{
			if (visueleInspectieAfbeelding != null)
			{
				hibernateService.delete(visueleInspectieAfbeelding);
				mammografie.setVisueleInspectieAfbeelding(null);
			}
		}
		hibernateService.saveOrUpdate(mammografie);
	}

	private MammaOnderzoek getOnderzoek(MammografieOpslaanDto action, MammaAfspraak afspraak)
	{
		if (afspraak == null)
		{
			throw new IllegalStateException("Afspraak id " + action.getAfspraakId() + " bestaat niet");
		}

		MammaOnderzoek onderzoek = afspraak.getOnderzoek();

		if (onderzoek == null || onderzoek.isDoorgevoerd())
		{
			throw new IllegalStateException("Opslaan visuele inspectie alleen mogelijk bij niet doorgevoerd onderzoek of onderzoek is null.");
		}
		return onderzoek;
	}
}
