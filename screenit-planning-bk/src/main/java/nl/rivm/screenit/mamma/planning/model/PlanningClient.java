package nl.rivm.screenit.mamma.planning.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-planning-bk
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

import java.math.BigDecimal;
import java.time.LocalDate;

import nl.rivm.screenit.mamma.planning.index.PlanningPostcodeReeksIndex;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.model.mamma.enums.MammaFactorType;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;

public final class PlanningClient extends PlanningEntiteit
{
	private final LocalDate geboorteDatum;

	private final int uitnodigenVanafJaar;

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

	private MammaUitstelReden uitstelReden;

	private PlanningStandplaats afspraakStandplaats;

	private final MammaFactorType factorType;

	private final PlanningTehuis tehuis;

	private final LocalDate laatsteScreeningRondeCreatieDatum;

	private final LocalDate laatsteMammografieAfgerondDatum;

	private final MammaCapaciteitBlokType blokType;

	private LocalDate vorigeScreeningRondeCreatieDatum;

	private boolean uitgenodigdHuidigeStandplaatsRonde = false;

	private boolean uitgenodigdHuidigeStandplaatsRondeIsGeforceerd = false;

	private boolean suspect;

	private LocalDate huidigeStreefDatum = null;

	public PlanningClient(Long id, LocalDate geboorteDatum, String postcode, boolean eersteOnderzoek, MammaDoelgroep doelgroep, BigDecimal deelnamekans,
		Boolean deelnamekansVervolgRonde, PlanningScreeningsOrganisatie screeningsOrganisatie, PlanningStandplaats uitstelStandplaats, LocalDate uitstelStreefDatum,
		Boolean uitgenodigdNaUitstel, MammaUitstelReden uitstelReden, PlanningStandplaats afspraakStandplaats, PlanningTehuis tehuis, LocalDate laatsteScreeningRondeCreatieDatum,
		LocalDate laatsteMammografieAfgerondDatum, boolean suspect)
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
		this.suspect = suspect;
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

	public LocalDate getHuidigeStreefDatum()
	{
		return huidigeStreefDatum;
	}

	public void setHuidigeStreefDatum(LocalDate huidigeStreefDatum)
	{
		this.huidigeStreefDatum = huidigeStreefDatum;
	}

	public LocalDate getGeboorteDatum()
	{
		return geboorteDatum;
	}

	public String getPostcode()
	{
		return postcode;
	}

	public MammaDoelgroep getDoelgroep()
	{
		return doelgroep;
	}

	public BigDecimal getDeelnamekans()
	{
		return deelnamekans;
	}

	public PlanningScreeningsOrganisatie getScreeningsOrganisatie()
	{
		return screeningsOrganisatie;
	}

	public PlanningBenodigd getBenodigd()
	{
		return benodigd;
	}

	public PlanningStandplaats getUitstelStandplaats()
	{
		return uitstelStandplaats;
	}

	public PlanningStandplaats getAfspraakStandplaats()
	{
		return afspraakStandplaats;
	}

	public void setAfspraakStandplaats(PlanningStandplaats afspraakStandplaats)
	{
		this.afspraakStandplaats = afspraakStandplaats;
	}

	public MammaFactorType getFactorType()
	{
		return factorType;
	}

	public int getUitnodigenVanafJaar()
	{
		return uitnodigenVanafJaar;
	}

	public int getUitnodigenTotEnMetJaar()
	{
		return uitnodigenTotEnMetJaar;
	}

	public int getVanafJaar()
	{
		return vanafJaar;
	}

	public int getTotEnMetJaar()
	{
		return totEnMetJaar;
	}

	public boolean isUitgenodigdHuidigeStandplaatsRonde()
	{
		return uitgenodigdHuidigeStandplaatsRonde;
	}

	public void setUitgenodigdHuidigeStandplaatsRonde(boolean uitgenodigdHuidigeStandplaatsRonde)
	{
		this.uitgenodigdHuidigeStandplaatsRonde = uitgenodigdHuidigeStandplaatsRonde;
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

	public LocalDate getVorigeScreeningRondeCreatieDatum()
	{
		return vorigeScreeningRondeCreatieDatum;
	}

	public boolean isUitgenodigdHuidigeStandplaatsRondeIsGeforceerd()
	{
		return uitgenodigdHuidigeStandplaatsRondeIsGeforceerd;
	}

	public void setUitgenodigdHuidigeStandplaatsRondeIsGeforceerd(boolean uitgenodigdHuidigeStandplaatsRondeIsGeforceerd)
	{
		this.uitgenodigdHuidigeStandplaatsRondeIsGeforceerd = uitgenodigdHuidigeStandplaatsRondeIsGeforceerd;
	}

	public LocalDate getLaatsteScreeningRondeCreatieDatum()
	{
		return laatsteScreeningRondeCreatieDatum;
	}

	public LocalDate getLaatsteMammografieAfgerondDatum()
	{
		return laatsteMammografieAfgerondDatum;
	}

	public LocalDate getUitstelStreefDatum()
	{
		return uitstelStreefDatum;
	}

	public PlanningTehuis getTehuis()
	{
		return tehuis;
	}

	public MammaCapaciteitBlokType getBlokType()
	{
		return blokType;
	}

	public boolean isSuspect()
	{
		return suspect;
	}

	public Boolean getUitgenodigdNaUitstel()
	{
		return uitgenodigdNaUitstel;
	}

	public void setUitgenodigdNaUitstel(Boolean uitgenodigdNaUitstel)
	{
		this.uitgenodigdNaUitstel = uitgenodigdNaUitstel;
	}

	public Boolean getDeelnamekansVervolgRonde()
	{
		return deelnamekansVervolgRonde;
	}

	public MammaUitstelReden getUitstelReden()
	{
		return uitstelReden;
	}
}
