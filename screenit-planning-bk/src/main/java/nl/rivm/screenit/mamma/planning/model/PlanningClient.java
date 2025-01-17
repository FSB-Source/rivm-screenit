package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;
import java.time.LocalDate;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.mamma.planning.index.PlanningPostcodeReeksIndex;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.model.mamma.enums.MammaUitnodigingsintervalType;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;

@Getter
@Setter
public final class PlanningClient extends PlanningEntiteit
{
	private final LocalDate geboorteDatum;

	private final int uitnodigenVanafJaar;

	private final LocalDate laatsteUitnodigingDatum;

	private final int uitnodigenTotEnMetJaar;

	private final int vanafJaar;

	private final int totEnMetJaar;

	private final String postcode;

	private final MammaDoelgroep doelgroep;

	private final BigDecimal deelnamekans;

	private final Boolean deelnamekansVervolgRonde;

	private final PlanningScreeningsOrganisatie screeningsOrganisatie;

	private final PlanningBenodigd benodigd = new PlanningBenodigd();

	private final PlanningStandplaats uitstelStandplaats;

	private final LocalDate uitstelStreefDatum;

	private Boolean uitgenodigdNaUitstel;

	private final MammaUitstelReden uitstelReden;

	private PlanningStandplaats afspraakStandplaats;

	private final MammaFactorType factorType;

	private final PlanningTehuis tehuis;

	private final LocalDate laatsteScreeningRondeCreatieDatum;

	private final LocalDate laatsteMammografieAfgerondDatum;

	private final MammaCapaciteitBlokType blokType;

	private LocalDate vorigeScreeningRondeCreatieDatum;

	private boolean uitgenodigdHuidigeStandplaatsRonde = false; 

	private boolean uitgenodigdHuidigeStandplaatsRondeIsGeforceerd = false;

	private boolean isNoShow = false;

	private final MammaUitnodigingsintervalType uitnodigingsintervalType;

	private LocalDate huidigeStreefDatum = null;

	public PlanningClient(Long id, LocalDate geboorteDatum, String postcode, boolean eersteOnderzoek, MammaDoelgroep doelgroep, BigDecimal deelnamekans,
		Boolean deelnamekansVervolgRonde, PlanningScreeningsOrganisatie screeningsOrganisatie, PlanningStandplaats uitstelStandplaats, LocalDate uitstelStreefDatum,
		Boolean uitgenodigdNaUitstel, MammaUitstelReden uitstelReden, PlanningStandplaats afspraakStandplaats, PlanningTehuis tehuis, LocalDate laatsteScreeningRondeCreatieDatum,
		LocalDate laatsteMammografieAfgerondDatum, MammaUitnodigingsintervalType uitnodigingsintervalType, LocalDate laatsteUitnodigingDatum)
	{
		super(id);
		this.geboorteDatum = geboorteDatum;
		this.postcode = postcode;
		this.doelgroep = doelgroep;
		this.deelnamekans = deelnamekans;
		this.deelnamekansVervolgRonde = deelnamekansVervolgRonde;
		this.screeningsOrganisatie = screeningsOrganisatie;
		this.uitstelStandplaats = uitstelStandplaats;
		this.uitstelStreefDatum = uitstelStreefDatum;
		this.uitgenodigdNaUitstel = uitgenodigdNaUitstel;
		this.afspraakStandplaats = afspraakStandplaats;
		this.tehuis = tehuis;
		this.laatsteScreeningRondeCreatieDatum = laatsteScreeningRondeCreatieDatum;
		this.laatsteMammografieAfgerondDatum = laatsteMammografieAfgerondDatum;
		this.laatsteUitnodigingDatum = laatsteUitnodigingDatum;
		this.uitnodigingsintervalType = uitnodigingsintervalType;
		this.uitstelReden = uitstelReden;

		uitnodigenVanafJaar = geboorteDatum.getYear() + PlanningConstanten.vanafLeeftijd;
		vanafJaar = getUitnodigenVanafJaar() > PlanningConstanten.plannenVanafJaar ? getUitnodigenVanafJaar() : PlanningConstanten.plannenVanafJaar;
		uitnodigenTotEnMetJaar = geboorteDatum.getYear() + PlanningConstanten.totEnMetLeeftijd;
		totEnMetJaar = uitnodigenTotEnMetJaar < PlanningConstanten.plannenTotEnMetJaar ? uitnodigenTotEnMetJaar : PlanningConstanten.plannenTotEnMetJaar;

		if (tehuis != null)
		{
			this.factorType = this.doelgroep == MammaDoelgroep.MINDER_VALIDE ? MammaFactorType.MINDER_VALIDE : MammaFactorType.DUBBELE_TIJD;
		}
		else
		{
			switch (this.doelgroep)
			{
			case REGULIER:
				if (eersteOnderzoek)
				{
					this.factorType = MammaFactorType.EERSTE_ONDERZOEK;
				}
				else
				{
					this.factorType = MammaFactorType.GEEN;
				}
				break;
			case DUBBELE_TIJD:
				this.factorType = MammaFactorType.DUBBELE_TIJD;
				break;
			case MINDER_VALIDE:
				this.factorType = MammaFactorType.MINDER_VALIDE;
				break;
			default:
				throw new IllegalArgumentException();
			}
		}

		blokType = tehuis != null ? MammaCapaciteitBlokType.TEHUIS : MammaCapaciteitBlokType.REGULIER;
	}

	public BigDecimal getGebruikteCapaciteit(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		return screeningsOrganisatie.getFactor(factorType);
	}

	public BigDecimal getBenodigdeCapaciteit(PlanningScreeningsOrganisatie screeningsOrganisatie)
	{
		return getGebruikteCapaciteit(screeningsOrganisatie).multiply(deelnamekans);
	}

	public PlanningStandplaats getStandplaats()
	{
		if (tehuis != null)
		{
			return tehuis.getStandplaats();
		}
		else
		{
			PlanningPostcodeReeks postcodeReeks = PlanningPostcodeReeksIndex.get(postcode);
			return postcodeReeks == null ? null : postcodeReeks.getStandplaats();
		}
	}

	public void setVorigeScreeningRondeCreatieDatum(LocalDate vorigeScreeningRondeCreatieDatum)
	{
		this.vorigeScreeningRondeCreatieDatum = vorigeScreeningRondeCreatieDatum;
		if (vorigeScreeningRondeCreatieDatum != null)
		{
			setHuidigeStreefDatum(vorigeScreeningRondeCreatieDatum.plusYears(PlanningConstanten.STREEF_INTERVAL));
		}
		else
		{
			setHuidigeStreefDatum(null);
		}
	}

	public boolean inTehuis()
	{
		return tehuis != null;
	}

	public boolean isSuspect()
	{
		return MammaUitnodigingsintervalType.isSuspect(uitnodigingsintervalType);
	}

	public boolean isSuspectOfHoogRisico()
	{
		return MammaUitnodigingsintervalType.isSuspectOfHoogRisico(uitnodigingsintervalType);
	}
}
