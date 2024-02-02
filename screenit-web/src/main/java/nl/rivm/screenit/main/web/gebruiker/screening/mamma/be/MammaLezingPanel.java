package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be;

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

import java.util.List;

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxButton;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding.MammaLaesiesAfbeeldingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.beoordelen.BeperktBeoordeelbaarPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.discrepantie_arbitrage.DiscrepantieArbitrageLezingenContainer;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.popup.LezingPdfDialogPanel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaLezingType;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import static nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding.MammaLaesiesAfbeeldingPanel.UNSAVED_DEFAULT_LEZING_ID;

public class MammaLezingPanel extends GenericPanel<MammaLezing>
{
	private final ScreenitForm<MammaLezing> form;

	private MammaLaesiesAfbeeldingPanel mammaAfbeeldingPanel;

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	private final LaesieDtoMapper laesieDtoMapper = new LaesieDtoMapper();

	private IndicatingAjaxButton arbitrageBtn;

	private IndicatingAjaxButton afrondenBtn;

	private final BootstrapDialog dialog;

	public MammaLezingPanel(GenericPanel<MammaBeoordeling> parentPanel, String id, IModel<MammaLezing> lezingModel, MammaLezingParameters lezingParameters)
	{
		super(id, lezingModel);

		addLaesiesAfbeelding(getModel(), lezingParameters.isInzien(), lezingParameters.isMetAfbeelding(), lezingParameters.getAmputatie());

		form = new ScreenitForm<>("birads_form");
		if (lezingParameters.isInzien())
		{
			form.add(new AttributeAppender("class", new Model<>("inactive-element all-radius not-tracked-for-changes-on-page"), " "));
			mammaAfbeeldingPanel.add(new AttributeAppender("class", new Model<>("inactive-element all-radius"), " "));
		}

		form.add(new Label("beoordeling_type", getHuidigePanelType(lezingModel) != null ? getHuidigePanelType(lezingModel).getNaam() : ""));

		boolean isAutoniem = !MammobridgeRole.anoniemeRollen().contains(ScreenitSession.get().getMammaHuidigeIDS7Role());
		form.add(new Label("beoordelaar.medewerker.voornaamAchternaam").setVisible(isAutoniem));

		form.add(new MammaBiradsKeuzePanel("biradskeuze", lezingModel, lezingParameters));

		form.add(new Label("textboxname", "Opmerking").setVisible(lezingParameters.isToonBiradsOpmerkingVeld()));
		ComponentHelper.addTextArea(form, "biradsOpmerking", false, 255, lezingParameters.isInzien()).setVisible(lezingParameters.isToonBiradsOpmerkingVeld());
		form.add(new BeperktBeoordeelbaarPanel("beperktBeoordeelbaarPanel", lezingModel, true));

		var tomosyntheseRelevantVoorBeoordeling = maakTomosyntheseRelevantVoorBeoordeling(lezingParameters, getHuidigePanelType(lezingModel));
		form.add(tomosyntheseRelevantVoorBeoordeling);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		if (parentPanel instanceof MammaReadOnlyLezingPanel)
		{
			form.add(new EmptyPanel("arbitrage").setVisible(false));
			form.add(new EmptyPanel("overnemen").setVisible(false));
			form.add(new EmptyPanel("afronden").setVisible(false));
			createVerslagButton(getHuidigePanelType(lezingModel), lezingParameters);
		}
		else if (parentPanel instanceof DiscrepantieArbitrageLezingenContainer)
		{
			createArbitrageButton((DiscrepantieArbitrageLezingenContainer) parentPanel, getHuidigePanelType(lezingModel), lezingParameters);
			createOvernemenButton((DiscrepantieArbitrageLezingenContainer) parentPanel, lezingParameters);
			createFormAfrondenButton((DiscrepantieArbitrageLezingenContainer) parentPanel, lezingParameters);
			createVerslagButton(getHuidigePanelType(lezingModel), lezingParameters);
		}
		else
		{
			form.add(new EmptyPanel("arbitrage").setVisible(false));
			form.add(new EmptyPanel("overnemen").setVisible(false));
			form.add(new EmptyPanel("afronden").setVisible(false));
			form.add(new EmptyPanel("verslagPdf").setVisible(false));
		}
		add(form);
	}

	private RadioChoice<Boolean> maakTomosyntheseRelevantVoorBeoordeling(MammaLezingParameters lezingParameters, MammaLezingType lezingType)
	{
		var tomosyntheseRelevantVoorBeoordeling = ComponentHelper.addHorizontaleBooleanRadioChoice("tomosyntheseRelevantVoorBeoordeling");
		tomosyntheseRelevantVoorBeoordeling.setSuffix("<span class=\"checkmark\"></span></label>");

		tomosyntheseRelevantVoorBeoordeling.setVisible(lezingParameters.isToonTomosyntheseSlicesRadioButtons() && MammaLezingType.VERSLAG_LEZING != lezingType);
		tomosyntheseRelevantVoorBeoordeling.setEnabled(!lezingParameters.isInzien());
		tomosyntheseRelevantVoorBeoordeling.setRequired(true);
		return tomosyntheseRelevantVoorBeoordeling;
	}

	private void createOvernemenButton(DiscrepantieArbitrageLezingenContainer parentPanel, MammaLezingParameters lezingParameters)
	{
		var overnemenBtn = new IndicatingAjaxButton("overnemen")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaLezing huidigeLezing = MammaLezingPanel.this.getModelObject();
				MammaLezing discrepantieArbitrageLezing = parentPanel.getHuidigeLezingModel().getObject();
				List<LaesieDto> laesieDtos = laesieDtoMapper.lezingToLaesieDtos(huidigeLezing);
				target.appendJavaScript(String.format("neemLezingOver('%1$s', '%2$s', '%3$s');", laesieDtoMapper.laesiesDtosToJson(laesieDtos),
					mapOpmerkingToJsonString(huidigeLezing.getBiradsOpmerking() != null ? huidigeLezing.getBiradsOpmerking() : ""),
					discrepantieArbitrageLezing.getId() != null ? discrepantieArbitrageLezing.getId() : UNSAVED_DEFAULT_LEZING_ID));
			}
		};
		if (!lezingParameters.isToonOvernemenKnop())
		{
			overnemenBtn.setVisible(false);
		}

		overnemenBtn.setOutputMarkupId(true);
		overnemenBtn.setOutputMarkupPlaceholderTag(true);
		form.add(overnemenBtn);
	}

	private String mapOpmerkingToJsonString(String opmerking)
	{
		try
		{
			ObjectMapper objectMapper = new ObjectMapper();
			return objectMapper.writeValueAsString(opmerking);
		}
		catch (JsonProcessingException e)
		{
			throw new IllegalStateException("Cannot convert opmerking to jsonstring", e);
		}
	}

	private void createVerslagButton(MammaLezingType lezingType, MammaLezingParameters lezingParameters)
	{
		var pdfBtn = new IndicatingAjaxLink<Void>("verslagPdf")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new LezingPdfDialogPanel(IDialog.CONTENT_ID, MammaLezingPanel.this.getModel())
				{
					@Override
					public void close(AjaxRequestTarget target)
					{
						dialog.close(target);
					}
				});
			}
		};
		if (!MammaLezingType.VERSLAG_LEZING.equals(lezingType) || !lezingParameters.isInzien())
		{
			pdfBtn.setVisible(false);
		}

		pdfBtn.setOutputMarkupId(true);
		form.add(pdfBtn);
	}

	private MammaLezingType getHuidigePanelType(IModel<MammaLezing> lezingModel)
	{
		return lezingModel.getObject().getLezingType();
	}

	private void createArbitrageButton(DiscrepantieArbitrageLezingenContainer panel, MammaLezingType panelType, MammaLezingParameters lezingParameters)
	{
		arbitrageBtn = new ScreenitIndicatingAjaxButton("arbitrage")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaLezing lezing = MammaLezingPanel.this.getModelObject();
				panel.beoordelingNaarArbitrageEnOpslaan(lezing, target);
			}
		};
		arbitrageBtn.setVisible(!lezingParameters.isInzien() && MammaLezingType.DISCREPANTIE_LEZING.equals(panelType)
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_IMS_KOPPELING, Actie.INZIEN));
		arbitrageBtn.setOutputMarkupId(true);
		arbitrageBtn.setOutputMarkupPlaceholderTag(true);
		form.add(arbitrageBtn);
	}

	private void createFormAfrondenButton(DiscrepantieArbitrageLezingenContainer panel, MammaLezingParameters lezingParameters)
	{
		afrondenBtn = new ScreenitIndicatingAjaxButton("afronden")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaLezing lezing = MammaLezingPanel.this.getModelObject();
				valideerBirads(lezing);
				if (!hasErrorMessage())
				{
					List<LaesieDto> laesieDtoList = MammaLezingPanel.this.mammaAfbeeldingPanel.getModelObject();
					panel.lezingOpslaan(lezing, target, laesieDtoList);
				}
			}

			private void valideerBirads(MammaLezing lezing)
			{
				if (lezing.getBiradsRechts() == null)
				{
					error("Birads rechts is verplicht.");
				}
				if (lezing.getBiradsLinks() == null)
				{
					error("Birads links is verplicht.");
				}
				if (!beoordelingService.isLezingValide(lezing, MammaLezingPanel.this.mammaAfbeeldingPanel.getModelObject()))
				{
					error(getString("error.lezing.annotatie.en.birads"));
				}
			}
		};
		afrondenBtn.setOutputMarkupId(true);
		afrondenBtn.setVisible(!lezingParameters.isInzien() && !lezingParameters.isVerbergAfrondKnop()
			&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_IMS_KOPPELING, Actie.INZIEN));
		form.add(afrondenBtn);
	}

	private void addLaesiesAfbeelding(IModel<MammaLezing> lezingModel, boolean alleenLezen, boolean metAfbeelding, MammaAmputatie amputatie)
	{
		LaesieDtoMapper mapper = new LaesieDtoMapper();
		IModel<List<LaesieDto>> result = new ListModel<>(mapper.lezingToLaesieDtos(lezingModel.getObject()));

		mammaAfbeeldingPanel = new MammaLaesiesAfbeeldingPanel("afbeelding", result, alleenLezen, lezingModel.getObject().getId(), false, amputatie);
		add(mammaAfbeeldingPanel.setVisible(metAfbeelding));
	}

	public void blokeerOpslaan(AjaxRequestTarget target)
	{
		afrondenBtn.setEnabled(false);
		arbitrageBtn.setEnabled(false);
		target.add(afrondenBtn);
		target.add(arbitrageBtn);
	}
}
