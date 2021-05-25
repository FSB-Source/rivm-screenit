package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.screeningseenheid;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.functionalinterfaces.StringResolver;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.validation.AbstractFormValidator;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public class TijdelijkeBeAanSeValidator extends AbstractFormValidator
{
	private final StringResolver resolver;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private final ScreenitDropdown<BeoordelingsEenheid> tijdelijkeBeoordelingsEenheid;

	private final ScreenitDropdown<BeoordelingsEenheid> beoordelingsEenheid;

	private final DatePicker tijdelijkeBeVanafDatum;

	private final DatePicker tijdelijkeBeTotEnMetDatum;

	private final MammaSEEditPage page;

	private long tijdelijkeBe;

	private LocalDate vanafDatum;

	private LocalDate totMetDatum;

	private long beId;

	public TijdelijkeBeAanSeValidator(MammaSEEditPage page, StringResolver resolver, ScreenitDropdown<BeoordelingsEenheid> tijdelijkeBeoordelingsEenheid,
		ScreenitDropdown<BeoordelingsEenheid> beoordelingsEenheid, DatePicker tijdelijkeBeVanafDatum, DatePicker tijdelijkeBeTotEnMetDatum)
	{
		this.page = page;
		this.resolver = resolver;
		this.tijdelijkeBeoordelingsEenheid = tijdelijkeBeoordelingsEenheid;
		this.beoordelingsEenheid = beoordelingsEenheid;
		this.tijdelijkeBeVanafDatum = tijdelijkeBeVanafDatum;
		this.tijdelijkeBeTotEnMetDatum = tijdelijkeBeTotEnMetDatum;
	}

	@Override
	public FormComponent<?>[] getDependentFormComponents()
	{
		return new FormComponent[] { tijdelijkeBeoordelingsEenheid, beoordelingsEenheid, tijdelijkeBeVanafDatum, tijdelijkeBeTotEnMetDatum };
	}

	@Override
	@SuppressWarnings("unchecked")
	public void validate(Form<?> form)
	{
		IModel<MammaScreeningsEenheid> se = (IModel<MammaScreeningsEenheid>) page.getDefaultModel();
		tijdelijkeBe = getBeIdFromForm(tijdelijkeBeoordelingsEenheid);
		beId = getBeIdFromForm(beoordelingsEenheid);
		vanafDatum = DateUtil.toLocalDate((Date) tijdelijkeBeVanafDatum.getConvertedInput());
		totMetDatum = DateUtil.toLocalDate((Date) tijdelijkeBeTotEnMetDatum.getConvertedInput());
		boolean isHuidigeTijdelijkeBeActief = checkRangeActiveHuidigeTijdelijkeBE(se.getObject().getTijdelijkeBeVanafDatum(),
			se.getObject().getTijdelijkeBeTotEnMetDatum());
		boolean isVanafDatumOngewijzigd = checkDatumOngewijzigd(se.getObject().getTijdelijkeBeVanafDatum(), vanafDatum);
		boolean isTotMetToekomstig = checkDatumToekomst(totMetDatum);
		boolean isVanafToekomstig = checkDatumToekomst(vanafDatum);
		boolean isTotMetDeHuidigeDag = checkDagIsVandaag(totMetDatum);
		boolean isVanafDeHuidigeDag = checkDagIsVandaag(vanafDatum);
		boolean isTijdelijkeBeOngewijzigd = checkBeOngewijzigd(se.getObject(), tijdelijkeBe);
		if (heeftGeenWijzigingen(vanafDatum, totMetDatum, tijdelijkeBe, se.getObject(), beId))
		{
			return;
		}
		else if (tijdelijkeBe == beId)
		{
			form.error(resolver.resolveString("error.beGelijkAanTijdelijkeBe"));
		}
		else if (vanafDatum == null && totMetDatum == null && tijdelijkeBe == 0L && !isHuidigeTijdelijkeBeActief)
		{
			return;
		}
		else if (vanafDatum == null && totMetDatum == null && tijdelijkeBe == 0L && isHuidigeTijdelijkeBeActief)
		{
			form.error(resolver.resolveString("error.tijdelijkeBePeriodeActief"));
		}
		else if (!checkVanafDatumVoorTotMetDatum(vanafDatum, totMetDatum))
		{
			if (totMetDatum == null)
			{
				form.error(resolver.resolveString("error.tijdelijkeBe.totMetDatumLeeg"));
			}
			else
			{
				form.error(resolver.resolveString("error.tijdelijkeBe.totEnMetDatumVoor"));
			}
		}
		else if (isHuidigeTijdelijkeBeActief && (!isVanafDatumOngewijzigd || (!isTotMetToekomstig && !isTotMetDeHuidigeDag) || !isTijdelijkeBeOngewijzigd))
		{
			if (!isTijdelijkeBeOngewijzigd)
			{
				form.error(resolver.resolveString("error.tijdelijkeBeVeranderd"));
			}
			else if (!isTotMetToekomstig && !isTotMetDeHuidigeDag)
			{
				form.error(resolver.resolveString("error.totMetDatumInVerleden"));
			}
			else if (!isVanafDatumOngewijzigd)
			{
				form.error(resolver.resolveString("error.vanafDatumGewijzigdBijUpdate"));
			}
		}
		else if (!isHuidigeTijdelijkeBeActief && (isVanafDeHuidigeDag || !isVanafToekomstig))
		{
			form.error(resolver.resolveString("error.vanafDatumIsHuidigeDag"));
		}
	}

	private boolean checkVanafDatumVoorTotMetDatum(LocalDate vanafDatum, LocalDate totMetDatum)
	{
		if (vanafDatum == null || totMetDatum == null)
		{
			return false;
		}
		return vanafDatum.toEpochDay() <= totMetDatum.toEpochDay();
	}

	private boolean checkBeOngewijzigd(MammaScreeningsEenheid se, long nieuweTijdelijkeBe)
	{
		long huidigeTijdelijkeBE = se.getTijdelijkeBeoordelingsEenheid() != null ? se.getTijdelijkeBeoordelingsEenheid().getId() : 0L;
		return huidigeTijdelijkeBE != 0L && huidigeTijdelijkeBE == nieuweTijdelijkeBe;
	}

	private boolean checkDagIsVandaag(LocalDate dag)
	{
		return dag != null && dag.toEpochDay() == currentDateSupplier.getLocalDate().toEpochDay();
	}

	private boolean checkDatumToekomst(LocalDate date)
	{
		return date != null && date.toEpochDay() > currentDateSupplier.getLocalDate().toEpochDay();
	}

	private boolean checkDatumOngewijzigd(Date currentDate, LocalDate date)
	{
		return currentDate != null && date != null
			&& DateUtil.toLocalDate(currentDate).toEpochDay() == date.toEpochDay();
	}

	private boolean checkRangeActiveHuidigeTijdelijkeBE(Date vanafDatum, Date totMetDatum)
	{
		if (vanafDatum == null || totMetDatum == null)
		{
			return false;
		}
		return DateUtil.isWithinRange(
			DateUtil.toLocalDate(vanafDatum),
			DateUtil.toLocalDate(totMetDatum),
			currentDateSupplier.getLocalDate());
	}

	private boolean heeftGeenWijzigingen(LocalDate vanafDatum, LocalDate totMetDatum, long tijdelijkeBe, MammaScreeningsEenheid se, long beId)
	{
		boolean vanafOngewijzigd = false, totMetOngewijzigd = false, tmpBeOngewijzigd = false, beOngewijzigd = false;
		if (vanafDatum != null && se.getTijdelijkeBeVanafDatum() != null)
		{
			vanafOngewijzigd = vanafDatum.toEpochDay() == DateUtil.toLocalDate(se.getTijdelijkeBeVanafDatum()).toEpochDay();
		}
		if (totMetDatum != null && se.getTijdelijkeBeTotEnMetDatum() != null)
		{
			totMetOngewijzigd = totMetDatum.toEpochDay() == DateUtil.toLocalDate(se.getTijdelijkeBeTotEnMetDatum()).toEpochDay();
		}
		if (tijdelijkeBe != 0L && se.getTijdelijkeBeoordelingsEenheid() != null)
		{
			tmpBeOngewijzigd = tijdelijkeBe == se.getTijdelijkeBeoordelingsEenheid().getId();
		}
		if (beId != 0L && se.getBeoordelingsEenheid() != null)
		{
			beOngewijzigd = beId == se.getBeoordelingsEenheid().getId();
		}
		return vanafOngewijzigd && totMetOngewijzigd && tmpBeOngewijzigd && beOngewijzigd;
	}

	private long getBeIdFromForm(ScreenitDropdown<BeoordelingsEenheid> beoordelingsEenheid)
	{
		try
		{
			if (StringUtils.isBlank(beoordelingsEenheid.getInput()))
			{
				return 0L;
			}
			int value = Integer.parseInt(beoordelingsEenheid.getInput());
			return beoordelingsEenheid.getChoices().get(value).getId();
		}
		catch (NumberFormatException | IndexOutOfBoundsException e)
		{
			return 0L;
		}
	}
}
