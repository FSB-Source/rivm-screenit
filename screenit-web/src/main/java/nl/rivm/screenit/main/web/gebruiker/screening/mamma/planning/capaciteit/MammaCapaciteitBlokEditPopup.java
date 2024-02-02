package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.capaciteit;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dto.mamma.planning.PlanningCapaciteitBlokDto;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.ScreenITDateTimeField;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaCapaciteitBlokType;
import nl.rivm.screenit.service.mamma.MammaBaseCapaciteitsBlokService;
import nl.rivm.screenit.util.BigDecimalUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.timefield.TimeField;
import nl.topicuszorg.wicket.planning.web.component.DateTimeField;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;

public abstract class MammaCapaciteitBlokEditPopup extends GenericPanel<PlanningCapaciteitBlokDto>
{

	private static final long serialVersionUID = 1L;

	private BootstrapDialog confirmPopup;

	private boolean magAanpassen;

	private WebMarkupContainer aantalOnderzoekenContainer;

	private WebMarkupContainer opmerkingenContainer;

	private WebMarkupContainer minderValideAfspraakMogelijkContainer;

	private IModel<String> reguliereOnderzoekenModel = Model.of("");

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaBaseCapaciteitsBlokService baseCapaciteitsBlokService;

	private final TimeField endTime = new TimeField("tot", true);

	public MammaCapaciteitBlokEditPopup(String id, IModel<PlanningCapaciteitBlokDto> model)
	{
		super(id, model);
		this.magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		confirmPopup = new BootstrapDialog("confirmPopup");
		confirmPopup.setOutputMarkupPlaceholderTag(true);
		add(confirmPopup);

		final Form<PlanningCapaciteitBlokDto> editForm = new Form<>("editForm", getModel());
		add(editForm);

		initForm(editForm);

		addOpslaanLink(editForm);
		addDeleteLink(editForm);
	}

	private void initForm(Form<PlanningCapaciteitBlokDto> form)
	{
		PlanningCapaciteitBlokDto blok = form.getModelObject();
		boolean isNieuw = blok.conceptId == null;

		String wijzigOfNieuw = "Wijzig";

		if (isNieuw)
		{
			wijzigOfNieuw = "Nieuw";
		}

		String tijdString = Constants.getDateTimeFormat().format(blok.vanaf) + "-" + Constants.getTimeFormat().format(blok.tot);

		WebMarkupContainer aantalReguliereOnderzoekenContainer = new WebMarkupContainer("aantalReguliereOnderzoekenContainer")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(!MammaCapaciteitBlokType.REGULIER.equals(getModelObject().blokType));
				if (getModelObject().blokType != null && !MammaCapaciteitBlokType.REGULIER.equals(getModelObject().blokType))
				{
					reguliereOnderzoekenModel.setObject(BigDecimalUtil.decimalToString(getBeschikbareCapaciteit(), 1));
				}
			}
		};

		Label aantalReguliereOnderzoekenLabel = new Label("aantalReguliereOnderzoeken", reguliereOnderzoekenModel);

		aantalReguliereOnderzoekenLabel.setOutputMarkupId(true);

		aantalOnderzoekenContainer = new WebMarkupContainer("aantalOnderzoekenContainer")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(!MammaCapaciteitBlokType.GEEN_SCREENING.equals(getModelObject().blokType));
			}
		};
		aantalOnderzoekenContainer.setOutputMarkupId(true);
		aantalOnderzoekenContainer.setOutputMarkupPlaceholderTag(true);

		form.add(aantalOnderzoekenContainer);

		opmerkingenContainer = new WebMarkupContainer("opmerkingenContainer")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(MammaCapaciteitBlokType.GEEN_SCREENING.equals(getModelObject().blokType));
			}
		};
		opmerkingenContainer.setOutputMarkupId(true);
		opmerkingenContainer.setOutputMarkupPlaceholderTag(true);

		ComponentHelper.addTextField(aantalOnderzoekenContainer, "aantalOnderzoeken", true, 10, Integer.class, false).add(RangeValidator.minimum(0))
			.add(new AjaxFormComponentUpdatingBehavior("keyup")
			{
				@Override
				protected void onUpdate(AjaxRequestTarget target)
				{
					if (getModelObject().blokType != null && !MammaCapaciteitBlokType.REGULIER.equals(getModelObject().blokType))
					{
						reguliereOnderzoekenModel.setObject(BigDecimalUtil.decimalToString(getBeschikbareCapaciteit(), 1));
						target.add(aantalReguliereOnderzoekenLabel);
					}
				}
			});
		aantalOnderzoekenContainer.add(aantalReguliereOnderzoekenContainer);
		aantalReguliereOnderzoekenContainer.add(aantalReguliereOnderzoekenLabel);

		TextArea<String> opmerkingen = ComponentHelper.addTextArea(opmerkingenContainer, "opmerkingen", true, 255,
			false);
		opmerkingenContainer.add(opmerkingen);

		form.add(opmerkingenContainer);

		CheckBox minderValideAfspraakMogelijk = ComponentHelper.newCheckBox("minderValideAfspraakMogelijk");

		minderValideAfspraakMogelijkContainer = new WebMarkupContainer("minderValideAfspraakMogelijkContainer")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(getModelObject().blokType != null && !getModelObject().blokType.equals(MammaCapaciteitBlokType.GEEN_SCREENING));
			}
		};
		minderValideAfspraakMogelijkContainer.add(minderValideAfspraakMogelijk);
		minderValideAfspraakMogelijkContainer.setOutputMarkupId(true);
		minderValideAfspraakMogelijkContainer.setOutputMarkupPlaceholderTag(true);
		form.add(minderValideAfspraakMogelijkContainer);

		ScreenitDropdown<MammaCapaciteitBlokType> bloktypen = ComponentHelper.newDropDownChoice("blokType",
			new ListModel<>(new ArrayList<>(Arrays.asList(MammaCapaciteitBlokType.values()))), new EnumChoiceRenderer<>(), true);

		bloktypen.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				target.add(aantalOnderzoekenContainer);
				target.add(opmerkingenContainer);
				target.add(minderValideAfspraakMogelijkContainer);
			}
		});

		form.add(bloktypen);

		form.add(new Label("actie", wijzigOfNieuw));
		form.add(new Label("screeningsEenheid.naam", hibernateService.get(MammaScreeningsEenheid.class, blok.screeningsEenheidId).getNaam()));
		form.add(new Label("tijd", tijdString));

		endTime.setOutputMarkupId(true);
		endTime.setEnabled(magAanpassen);
		endTime.setRequired(true);
		form.add(endTime);
		final DateTimeField startTime = new ScreenITDateTimeField("vanaf")
		{

			@Override
			public String getDatePickerLabel()
			{
				return "'Starttijd datum'";
			}

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				setDateForEndTimeField();

			}
		};

		startTime.setOutputMarkupId(true);
		startTime.setRequired(true);
		startTime.setEnabled(magAanpassen);
		form.add(startTime);
	}

	private BigDecimal getBeschikbareCapaciteit()
	{
		PlanningCapaciteitBlokDto blok = getModelObject();
		ScreeningOrganisatie screeningOrganisatie = ScreenitSession.get().getScreeningOrganisatie();

		if (blok.aantalOnderzoeken != null)
		{
			return blok.blokType.getFactorType().getFactor(screeningOrganisatie).multiply(new BigDecimal(blok.aantalOnderzoeken)).setScale(1, RoundingMode.HALF_UP);
		}
		return BigDecimal.ZERO;
	}

	private void setDateForEndTimeField()
	{

		var endDateTime = DateUtil.toLocalDateTime(endTime.getDate());
		var currentStartDate = DateUtil.toLocalDateTime(getModelObject().vanaf);

		if (endDateTime.getHour() == 0 && endDateTime.getMinute() == 0)
		{
			endDateTime = endDateTime.withDayOfYear(currentStartDate.plusDays(1).getDayOfYear());
		}
		else
		{
			endDateTime = endDateTime.withDayOfYear(currentStartDate.getDayOfYear());
		}
		endTime.setDate(DateUtil.toUtilDate(endDateTime));
	}

	private void addOpslaanLink(final Form<PlanningCapaciteitBlokDto> editForm)
	{
		IndicatingAjaxButton opslaanLink = new ConfirmingIndicatingAjaxSubmitLink<Void>("opslaan", editForm, confirmPopup, "opslaan.popup")
		{
			private int aantalAfspraken = 0;

			@Override
			protected boolean skipConfirmation()
			{
				IModel<PlanningCapaciteitBlokDto> blokModel = MammaCapaciteitBlokEditPopup.this.getModel();
				aantalAfspraken = baseCapaciteitsBlokService.getAantalAfsprakenOpBlok(blokModel.getObject(), false);
				return aantalAfspraken == 0;
			}

			@Override
			protected IModel<String> getContentStringModel()
			{
				if (aantalAfspraken > 0)
				{
					return Model.of(String.format(getString("opslaan.popup.content"), aantalAfspraken));
				}
				else
				{
					return Model.of();
				}

			}

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				setDateForEndTimeField();
				onOpslaan(target, MammaCapaciteitBlokEditPopup.this.getModel());
			}

		};

		add(opslaanLink);
		opslaanLink.setVisible(magAanpassen);
		opslaanLink.add(new Label("opslaanTekst", getOpslaanTekst()));
	}

	protected abstract void onOpslaan(AjaxRequestTarget target, IModel<PlanningCapaciteitBlokDto> model);

	private void addDeleteLink(final Form<PlanningCapaciteitBlokDto> editForm)
	{
		final IndicatingAjaxLink<Void> deleteSubmit = new ConfirmingIndicatingAjaxLink<Void>("verwijderen", confirmPopup, "verwijder.popup")
		{
			@Override
			protected IModel<String> getContentStringModel()
			{
				IModel<PlanningCapaciteitBlokDto> blokModel = MammaCapaciteitBlokEditPopup.this.getModel();
				int aantalAfspraken = baseCapaciteitsBlokService.getAantalAfsprakenOpBlok(blokModel.getObject(), true);
				if (aantalAfspraken > 0)
				{
					return Model.of(String.format(getString("verwijder.popup.content"), aantalAfspraken));
				}
				else
				{
					return Model.of();
				}

			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onVerwijderen(target, MammaCapaciteitBlokEditPopup.this.getModel());
			}

		};
		add(deleteSubmit);

		PlanningCapaciteitBlokDto blok = getModelObject();
		deleteSubmit.setVisible(blok.conceptId != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.VERWIJDEREN));
		deleteSubmit.add(new Label("verwijderenTekst", getDeleteTekst()));
	}

	protected abstract void onVerwijderen(AjaxRequestTarget target, IModel<PlanningCapaciteitBlokDto> model);

	protected IModel<String> getDeleteTekst()
	{
		return Model.of("Verwijderen");
	}

	protected IModel<String> getOpslaanTekst()
	{
		return Model.of("Onthouden");
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(reguliereOnderzoekenModel);
	}

}
