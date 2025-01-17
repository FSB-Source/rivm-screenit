package nl.rivm.screenit.specification.mamma;

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

import java.util.Date;

import javax.persistence.criteria.JoinType;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SingleTableHibernateObject_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.berichten.enums.VerslagStatus;
import nl.rivm.screenit.model.berichten.enums.VerslagType;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag_;
import nl.rivm.screenit.model.mamma.MammaFollowUpVerslag;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde_;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.verslag.MammaVerslag_;
import nl.rivm.screenit.specification.RangeSpecification;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject_;

import org.springframework.data.jpa.domain.Specification;

import com.google.common.collect.Range;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.SpecificationUtil.skipWhenNull;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaFollowUpRadiologieVerslagSpecification
{

	public static Specification<MammaFollowUpRadiologieVerslag> heeftOpenstaandeRadioVerslagenVanInstelling(Instelling instelling)
	{
		return (r, q, cb) ->
			cb.and(
				cb.isNull(r.get(MammaFollowUpRadiologieVerslag_.ingevoerdDoor)),
				cb.equal(r.get(MammaFollowUpRadiologieVerslag_.aangemaaktIn), instelling));
	}

	public static Specification<MammaFollowUpRadiologieVerslag> filterLaatsteRadioVerslagenOpBeoordelingStatus(MammaBeoordelingStatus beoordelingStatus)
	{
		return (r, q, cb) ->
		{
			var rondeJoin = join(r, MammaFollowUpRadiologieVerslag_.screeningRonde);
			var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);
			var laatsteOnderzoekJoin = join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var laatsteBeoordelingJoin = join(laatsteOnderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);

			var beoordelingPredicate = cb.equal(laatsteBeoordelingJoin, dossierJoin.get(MammaDossier_.laatsteBeoordelingMetUitslag));

			if (beoordelingStatus == null)
			{
				return beoordelingPredicate;
			}

			return cb.and(
				beoordelingPredicate,
				MammaBeoordelingSpecification.heeftStatus(beoordelingStatus).with(root -> laatsteBeoordelingJoin).toPredicate(r, q, cb)
			);
		};
	}

	public static Specification<MammaFollowUpRadiologieVerslag> heeftLaatsteBeoordelingMetUitslag()
	{
		return (r, q, cb) ->
		{
			var rondeJoin = join(r, MammaFollowUpRadiologieVerslag_.screeningRonde);
			var dossierJoin = join(rondeJoin, MammaScreeningRonde_.dossier);
			var laatsteOnderzoekJoin = join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			var laatsteBeoordelingJoin = join(laatsteOnderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);

			return cb.equal(laatsteBeoordelingJoin.get(AbstractHibernateObject_.id), dossierJoin.get(MammaDossier_.laatsteBeoordelingMetUitslag));
		};
	}

	public static Specification<MammaFollowUpRadiologieVerslag> heeftGeenIngevoerdDoor()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaFollowUpRadiologieVerslag_.ingevoerdDoor));
	}

	public static Specification<MammaFollowUpRadiologieVerslag> filterOpBeoordelingStatus(MammaBeoordelingStatus beoordelingStatus)
	{
		return skipWhenNull(beoordelingStatus, MammaBeoordelingSpecification.heeftStatus(beoordelingStatus).with(r ->
		{
			var rondeJoin = join(r, MammaFollowUpRadiologieVerslag_.screeningRonde);
			var laatsteOnderzoekJoin = join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);
			return join(laatsteOnderzoekJoin, MammaOnderzoek_.laatsteBeoordeling);
		}));
	}

	public static Specification<MammaFollowUpRadiologieVerslag> filterOpScreeningOrganisatie(ScreeningOrganisatie screeningOrganisatie)
	{
		return skipWhenNull(screeningOrganisatie, (r, q, cb) ->
		{
			var organisatieJoin = r.join(MammaFollowUpRadiologieVerslag_.aangemaaktIn);
			var parentJoin = join(organisatieJoin, Instelling_.parent, JoinType.LEFT);

			return cb.or(
				cb.equal(organisatieJoin.get(Instelling_.parent), screeningOrganisatie.getId()),
				cb.equal(parentJoin.get(Instelling_.parent), screeningOrganisatie.getId())
			);
		});

	}

	public static Specification<MammaFollowUpRadiologieVerslag> isAangemaaktInAfdeling(Instelling afdeling)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaFollowUpRadiologieVerslag_.aangemaaktIn), afdeling);
	}

	public static Specification<MammaFollowUpRadiologieVerslag> heeftAangemaaktOpOfVoor(Date aangemaaktOp)
	{
		return (r, q, cb) -> cb.lessThanOrEqualTo(r.get(MammaFollowUpRadiologieVerslag_.aangemaaktOp), aangemaaktOp);
	}

	public static Specification<MammaFollowUpRadiologieVerslag> filterOnderzoekCreatieDatumAangemaaktTussen(Range<Date> range)
	{

		return skipWhenNull(range, (r, q, cb) ->
		{
			var rondeJoin = join(r, MammaFollowUpRadiologieVerslag_.screeningRonde);
			var laatsteOnderzoekJoin = join(rondeJoin, MammaScreeningRonde_.laatsteOnderzoek);

			return RangeSpecification.bevat(range, m -> laatsteOnderzoekJoin.get(MammaOnderzoek_.creatieDatum)).toPredicate(r, q, cb);
		});
	}

	public static Specification<MammaFollowUpRadiologieVerslag> heeftRondeZonderPaVerslag()
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = join(r, MammaFollowUpRadiologieVerslag_.screeningRonde);

			var subquery = q.subquery(Long.class);
			var subRoot = subquery.from(MammaFollowUpVerslag.class);
			subquery.select(subRoot.get(SingleTableHibernateObject_.id))
				.where(cb.equal(subRoot.get(MammaVerslag_.status), VerslagStatus.AFGEROND),
					cb.equal(subRoot.get(MammaVerslag_.screeningRonde), screeningRondeJoin),
					cb.equal(subRoot.get(MammaVerslag_.type), VerslagType.MAMMA_PA_FOLLOW_UP));

			return cb.and(
				cb.not(cb.exists(subquery)));
		};
	}

	public static Specification<MammaFollowUpRadiologieVerslag> isPaNogTeVerwachtenEnPathologieUitgevoerd()
	{
		return (r, q, cb) ->
			cb.and(
				cb.isNull(r.get(MammaFollowUpRadiologieVerslag_.paVerslagNietTeVerwachten)),
				cb.isTrue(r.get(MammaFollowUpRadiologieVerslag_.pathologieUitgevoerd)));
	}

	public static Specification<MammaFollowUpRadiologieVerslag> heeftGeenFollowUpConclusieStatus()
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = join(r, MammaFollowUpRadiologieVerslag_.screeningRonde);
			return cb.isNull(screeningRondeJoin.get(MammaScreeningRonde_.followUpConclusieStatus));
		};
	}

	public static Specification<MammaFollowUpRadiologieVerslag> heeftScreeningRondeGelijkMetUitnodiging()
	{
		return (r, q, cb) ->
		{
			var screeningRondeJoin = join(r, MammaFollowUpRadiologieVerslag_.screeningRonde);
			var dossierJoin = join(screeningRondeJoin, MammaScreeningRonde_.dossier);
			var beoordelingJoin = join(dossierJoin, MammaDossier_.laatsteBeoordelingMetUitslag);
			var onderzoekJoin = join(beoordelingJoin, MammaBeoordeling_.onderzoek);
			var afspraakJoin = join(onderzoekJoin, MammaOnderzoek_.afspraak);
			var uitnodigingJoin = join(afspraakJoin, MammaAfspraak_.uitnodiging);
			return cb.equal(uitnodigingJoin.get(MammaUitnodiging_.screeningRonde), screeningRondeJoin.get(TablePerClassHibernateObject_.id));
		};

	}

}
