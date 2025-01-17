
package nl.rivm.screenit.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;

import javax.persistence.criteria.From;
import javax.persistence.criteria.JoinType;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.Uitnodiging;
import nl.rivm.screenit.model.cervix.CervixDossier_;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde_;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitnodiging_;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonScreeningRonde_;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonUitnodiging_;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.gba.GbaMutatie;
import nl.rivm.screenit.model.logging.LoggingZoekCriteria;
import nl.rivm.screenit.repository.cervix.CervixUitnodigingRepository;
import nl.rivm.screenit.repository.colon.ColonUitnodigingRepository;
import nl.rivm.screenit.service.BaseUitnodigingService;
import nl.rivm.screenit.service.ClientService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.specification.algemeen.AdresSpecification;
import nl.rivm.screenit.specification.cervix.CervixUitnodigingSpecification;
import nl.rivm.screenit.specification.colon.ColonUitnodigingSpecification;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@Service
public class BaseUitnodigingServiceImpl implements BaseUitnodigingService
{

	@Autowired
	private LogService logService;

	@Autowired
	private ClientService clientService;

	@Autowired
	private CervixUitnodigingRepository cervixUitnodigingRepository;

	@Autowired
	private ColonUitnodigingRepository colonUitnodigingRepository;

	@Autowired
	private HibernateService hibernateService;

	@Override
	public <U extends Uitnodiging<?>> boolean heeftAlEenNieuwereUitnodiging(U huidigeUitnodiging)
	{
		boolean heeftAlEenNieuwereUitnodiging = false;
		for (Uitnodiging<?> uitnodiging : huidigeUitnodiging.getScreeningRonde().getUitnodigingen())
		{
			if (DateUtil.compareAfter(uitnodiging.getCreatieDatum(), huidigeUitnodiging.getCreatieDatum()))
			{
				heeftAlEenNieuwereUitnodiging = true;
				break;
			}
		}
		return heeftAlEenNieuwereUitnodiging;
	}

	@Override
	@Transactional(readOnly = true, propagation = Propagation.SUPPORTS)
	public <U extends InpakbareUitnodiging<?>> boolean isVerstuurdMetTijdelijkAdres(U uitnodiging)
	{
		var verstuurdDatum = DateUtil.toLocalDate(uitnodiging.getVerstuurdDatum());
		return AdresUtil.isTijdelijkAdres(uitnodiging.getScreeningRonde().getDossier().getClient().getPersoon(), verstuurdDatum);
	}

	@Override
	public <U extends InpakbareUitnodiging<?>> boolean isAdresGewijzigdNaUitnodigingsdatum(U uitnodiging)
	{
		boolean isAdresGewijzigd = false;
		Client client = uitnodiging.getScreeningRonde().getDossier().getClient();
		boolean kanVersturenMetTijdelijkAdres = clientService.isTijdelijkeAdresNuActueel(client.getPersoon());
		boolean verstuurdMetTijdelijkAdres = isVerstuurdMetTijdelijkAdres(uitnodiging);

		isAdresGewijzigd |= verstuurdMetTijdelijkAdres && !kanVersturenMetTijdelijkAdres;

		if (!isAdresGewijzigd)
		{

			isAdresGewijzigd |= !verstuurdMetTijdelijkAdres && kanVersturenMetTijdelijkAdres;
		}

		if (!isAdresGewijzigd && verstuurdMetTijdelijkAdres && kanVersturenMetTijdelijkAdres)
		{

			LoggingZoekCriteria loggingZoekCriteria = new LoggingZoekCriteria();

			loggingZoekCriteria.setBsnClient(client.getPersoon().getBsn());
			loggingZoekCriteria.setGebeurtenis(Arrays.asList(LogGebeurtenis.WIJZIG_TIJDELIJK_ADRES));
			loggingZoekCriteria.setVanaf(uitnodiging.getVerstuurdDatum());
			isAdresGewijzigd |= logService.countLogRegels(loggingZoekCriteria) > 0;
		}

		if (!isAdresGewijzigd)
		{
			for (GbaMutatie mutatie : client.getGbaMutaties())
			{
				if (DateUtil.compareAfter(mutatie.getMutatieDatum(), uitnodiging.getVerstuurdDatum()))
				{
					isAdresGewijzigd = true;
					break;
				}
			}
		}
		return isAdresGewijzigd;
	}

	@Override
	public ColonUitnodiging getColonUitnodiging(String trackId, String postcode, Integer huisnummer)
	{
		return colonUitnodigingRepository.findOne(
			AdresSpecification.heeftPostcode(postcode).and(AdresSpecification.heeftHuisnummer(huisnummer))
				.with((From<?, ? extends ColonUitnodiging> r) -> join(colonPersoonJoin(r), GbaPersoon_.gbaAdres))
				.or(AdresSpecification.heeftPostcode(postcode).and(AdresSpecification.heeftHuisnummer(huisnummer))
					.with((From<?, ? extends ColonUitnodiging> r) -> join(colonPersoonJoin(r), GbaPersoon_.tijdelijkAdres, JoinType.LEFT)))
				.and(ColonUitnodigingSpecification.heeftTrackTraceId(trackId))).orElse(null);
	}

	@Override
	public CervixUitnodiging getCervixUitnodiging(String trackId, String postcode, Integer huisnummer)
	{
		return cervixUitnodigingRepository.findOne(AdresSpecification.heeftPostcode(postcode).and(AdresSpecification.heeftHuisnummer(huisnummer))
			.with((From<?, ? extends CervixUitnodiging> r) -> join(cervixPersoonJoin(r), GbaPersoon_.gbaAdres))
			.or(AdresSpecification.heeftPostcode(postcode).and(AdresSpecification.heeftHuisnummer(huisnummer))
				.with((From<?, ? extends CervixUitnodiging> r) -> join(cervixPersoonJoin(r), GbaPersoon_.tijdelijkAdres, JoinType.LEFT)))
			.and(CervixUitnodigingSpecification.heeftTrackTraceId(trackId))).orElse(null);
	}

	@Override
	public boolean colonUitnodigingExists(String trackId)
	{
		if (StringUtils.isBlank(trackId))
		{
			return false;
		}
		return colonUitnodigingRepository.exists(ColonUitnodigingSpecification.heeftTrackTraceId(trackId));
	}

	@Override
	public boolean cervixUitnodigingExists(String trackId)
	{
		if (StringUtils.isBlank(trackId))
		{
			return false;
		}
		return cervixUitnodigingRepository.exists(CervixUitnodigingSpecification.heeftTrackTraceId(trackId));
	}

	private From<?, ? extends GbaPersoon> colonPersoonJoin(From<?, ? extends ColonUitnodiging> r)
	{
		var rondeJoin = join(r, ColonUitnodiging_.screeningRonde);
		var dossierJoin = join(rondeJoin, ColonScreeningRonde_.dossier);
		var clientJoin = join(dossierJoin, ColonDossier_.client);
		return join(clientJoin, Client_.persoon);
	}

	private From<?, ? extends GbaPersoon> cervixPersoonJoin(From<?, ? extends CervixUitnodiging> r)
	{
		var rondeJoin = join(r, CervixUitnodiging_.screeningRonde);
		var dossierJoin = join(rondeJoin, CervixScreeningRonde_.dossier);
		var clientJoin = join(dossierJoin, CervixDossier_.client);
		return join(clientJoin, Client_.persoon);
	}

}
