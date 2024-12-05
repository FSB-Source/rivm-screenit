package nl.rivm.screenit.batch.jobs.mamma.uitnodigen.interval;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-bk
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
import java.util.Date;
import java.util.List;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import lombok.AllArgsConstructor;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.batch.jobs.helpers.BaseSpecificationScrollableResultReader;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;
import nl.rivm.screenit.specification.HibernateObjectSpecification;
import nl.rivm.screenit.specification.algemeen.PersoonSpecification;
import nl.rivm.screenit.specification.mamma.MammaAfspraakSpecification;
import nl.rivm.screenit.specification.mamma.MammaBaseDossierSpecification;
import nl.rivm.screenit.specification.mamma.MammaBeoordelingSpecification;
import nl.rivm.screenit.specification.mamma.MammaOnderzoekSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.ClientSpecification.voldoetAanMammaClientSelectieRestricties;
import static nl.rivm.screenit.specification.mamma.MammaBaseDossierSpecification.woontNietInTehuis;
import static nl.rivm.screenit.specification.mamma.MammaVolgendeUitnodigingSpecification.heeftPeildatumOpOfVoorBerekendeReferentieDatum;
import static org.springframework.data.jpa.domain.Specification.not;

@Component
@AllArgsConstructor
public class MammaIntervalUitnodigenReader extends BaseSpecificationScrollableResultReader<Client>
{
	private final MammaVolgendeUitnodigingService volgendeUitnodigingService;

	private final SimplePreferenceService preferenceService;

	private final ICurrentDateSupplier currentDateSupplier;

	@Override
	protected Specification<Client> createSpecification()
	{
		volgendeUitnodigingService.updateIntervalReferentieDatums();
		var vandaag = currentDateSupplier.getLocalDate();

		return heeftPeildatumOpOfVoorBerekendeReferentieDatum().<Client> withRoot(r -> join(dossierJoin(r), MammaDossier_.volgendeUitnodiging))
			.and(woontNietInTehuis().withRoot(this::dossierJoin))
			.and(voldoetAanMammaClientSelectieRestricties())
			.and(valtBinnenLeeftijdGrens(vandaag))
			.and(mammografieDatumIsOuderDanMaximumOfIsNull(vandaag))
			.and(heeftGeenToekomstigeAfspraak(vandaag))
			.and(heeftGeenLopendOnderzoek())
			.and(heeftGeenOpenstaandeBeoordeling());
	}

	private Specification<Client> mammografieDatumIsOuderDanMaximumOfIsNull(LocalDate vandaag)
	{
		return MammaBaseDossierSpecification.heeftNooitMammografieGehad()
			.or(MammaBaseDossierSpecification.heeftLaatsteMammografieAfgerondVoorMoment(DateUtil.toLocalDateTime(maximaleMammografieDatum(vandaag)))).withRoot(this::dossierJoin);
	}

	public Specification<Client> valtBinnenLeeftijdGrens(LocalDate vandaag)
	{
		var vanafLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_LEEFTIJD.name());
		var totEnMetLeeftijd = preferenceService.getInteger(PreferenceKey.MAMMA_MAXIMALE_LEEFTIJD.name());
		return PersoonSpecification.valtBinnenLeeftijd(vanafLeeftijd, totEnMetLeeftijd, vandaag).with(Client_.persoon);
	}

	private Specification<Client> heeftGeenToekomstigeAfspraak(LocalDate vandaag)
	{
		return not(MammaAfspraakSpecification.heeftStatus(MammaAfspraakStatus.GEPLAND).withRoot(this::afspraakJoin)).or(
			MammaAfspraakSpecification.valtInDatumPeriode(Range.lessThan(vandaag)).or(HibernateObjectSpecification.heeftGeenId()).withRoot(this::afspraakJoin));
	}

	private Specification<Client> heeftGeenLopendOnderzoek()
	{
		var heeftGeenOnderzoek = HibernateObjectSpecification.heeftGeenId().withRoot(this::onderzoekJoin);
		var heeftGeenLopendOnderzoek = MammaOnderzoekSpecification.heeftStatusIn(
				List.of(MammaOnderzoekStatus.AFGEROND, MammaOnderzoekStatus.ONVOLLEDIG, MammaOnderzoekStatus.ONDERBROKEN_ZONDER_VERVOLG))
			.and(MammaOnderzoekSpecification.isDoorgevoerd(true)).withRoot(this::onderzoekJoin);
		return heeftGeenOnderzoek.or(heeftGeenLopendOnderzoek);
	}

	private Specification<Client> heeftGeenOpenstaandeBeoordeling()
	{
		var heeftGeenBeoordeling = HibernateObjectSpecification.heeftGeenId().withRoot(this::beoordelingJoin);
		var heeftBeoordelingInAfgerondeStatus = MammaBeoordelingSpecification.heeftUitslagStatus().withRoot(this::beoordelingJoin);
		return heeftGeenBeoordeling.or(heeftBeoordelingInAfgerondeStatus);

	}

	private Date maximaleMammografieDatum(LocalDate vandaag)
	{
		int minimaleIntervalMammografieOnderzoeken = preferenceService.getInteger(PreferenceKey.MAMMA_MINIMALE_INTERVAL_MAMMOGRAFIE_ONDERZOEKEN.name());
		return DateUtil.toUtilDate(vandaag.minusDays(minimaleIntervalMammografieOnderzoeken));
	}

	private Join<Client, MammaDossier> dossierJoin(Root<Client> r)
	{
		return join(r, Client_.mammaDossier);
	}

	private Join<MammaDossier, MammaScreeningRonde> rondeJoin(Root<Client> r)
	{
		return join(dossierJoin(r), MammaDossier_.laatsteScreeningRonde, JoinType.LEFT);
	}

	private Join<MammaScreeningRonde, MammaOnderzoek> onderzoekJoin(Root<Client> r)
	{
		return join(rondeJoin(r), MammaScreeningRonde_.laatsteOnderzoek, JoinType.LEFT);
	}

	private Join<MammaOnderzoek, MammaBeoordeling> beoordelingJoin(Root<Client> r)
	{
		return join(onderzoekJoin(r), MammaOnderzoek_.laatsteBeoordeling, JoinType.LEFT);
	}

	private Join<MammaScreeningRonde, MammaUitnodiging> uitnodigingJoin(Root<Client> r)
	{
		return join(rondeJoin(r), MammaScreeningRonde_.laatsteUitnodiging, JoinType.LEFT);
	}

	private Join<MammaUitnodiging, MammaAfspraak> afspraakJoin(Root<Client> r)
	{
		return join(uitnodigingJoin(r), MammaUitnodiging_.laatsteAfspraak, JoinType.LEFT);
	}

}
