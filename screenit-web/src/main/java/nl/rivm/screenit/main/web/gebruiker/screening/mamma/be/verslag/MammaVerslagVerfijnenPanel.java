package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag;

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

import java.io.Serializable;
import java.util.List;
import java.util.function.Consumer;

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxButton;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaLezingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.MammaLezingParameters;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding.MammaLaesiesAfbeeldingPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies.ArchitectuurverstoringLaesiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies.AsymmetrieLaesiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies.CalcificatiesLaesiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies.LaesiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies.MassaLaesiePanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel.MammaNevenbevindingViewerPanel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseLezingService;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaVerslagVerfijnenPanel extends GenericPanel<MammaLezing>
{
	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	private MammaBaseLezingService baseLezingService;

	private final WebMarkupContainer laesiesContainer;

	private IModel<List<LaesieDto>> laesieDtos;

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	private IndicatingAjaxButton maakVerslagBtn;

	public MammaVerslagVerfijnenPanel(MammaVerslagRondePanel verslagRondePanel, String id, IModel<MammaLezing> model, MammaAmputatie amputatie, boolean toonAfwijkingSliceButtons)
	{
		super(id, model);
		ScreenitForm<MammaLezing> form = new ScreenitForm<>("verfijnenForm");
		add(form);
		renderAfkeurRedenen(form);

		LaesieDtoMapper mapper = new LaesieDtoMapper();
		laesieDtos = new ListModel<>(mapper.lezingToLaesieDtos(model.getObject()));
		MammaLaesiesAfbeeldingPanel afbeelding = new MammaLaesiesAfbeeldingPanel("verfijnVerslagAfbeelding", laesieDtos, false, model.getObject().getId(), true, amputatie);
		afbeelding.setOnAfbeeldingGewijzigd((Consumer<AjaxRequestTarget> & Serializable) this::onAfbeeldingGewijzigd);
		form.add(afbeelding);

		MammaLezingParameters lezingParameters = new MammaLezingParameters().setVerbergAfrondKnop(true)
			.setAmputatie(amputatie)
			.setMetAfbeelding(false)
			.setToonBiradsOpmerkingVeld(false);
		form.add(new MammaLezingPanel(null, "birads", model, lezingParameters));

		ComponentHelper.addTextArea(form, "biradsOpmerking", false, 255, lezingParameters.isInzien());

		maakTomosyntheseSliceButtons(toonAfwijkingSliceButtons, form);

		laesiesContainer = new WebMarkupContainer("laesiesContainer");
		laesiesContainer.add(createLaesieLijst());
		laesiesContainer.setOutputMarkupId(true);
		form.add(laesiesContainer);

		maakVerslagBtn = new ScreenitIndicatingAjaxButton("maakVerslag")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				valideerBirads(MammaVerslagVerfijnenPanel.this.getModelObject());
				if (!MammaVerslagVerfijnenPanel.this.hasErrorMessage())
				{
					IModel<MammaLezing> verslagLezingModel = MammaVerslagVerfijnenPanel.this.getModel();
					koppelLaesiesAanLezingEnSlaOp(verslagLezingModel, verslagRondePanel);
					BasePage.markeerFormulierenOpgeslagen(target);
					gaNaarVerwijsVerslagPanel(target, verslagRondePanel, verslagLezingModel, amputatie, toonAfwijkingSliceButtons);
				}
			}
		};
		maakVerslagBtn.setOutputMarkupId(true);
		maakVerslagBtn.setOutputMarkupPlaceholderTag(true);
		maakVerslagBtn.setEnabled(!verslagRondePanel.isVerslagLezingGeblokeerd());
		form.add(maakVerslagBtn);

		form.add(new MammaNevenbevindingViewerPanel("nevenbevindingen", verslagRondePanel.getModel()));
	}

	private void maakTomosyntheseSliceButtons(Boolean toonAfwijkingSliceButtons, ScreenitForm<MammaLezing> form)
	{
		var afwijkingTeZienOpContainer = new MammaAfwijkingTeZienOpPanel("teZienOpContainer", getModel());
		afwijkingTeZienOpContainer.setVisible(toonAfwijkingSliceButtons);
		form.add(afwijkingTeZienOpContainer);

		var zijde = baseLezingService.bepaalZijdeMetPrioriteit(getModelObject()).getNaam();

		var sliceTomosyntheseTekst = new Label("sliceTomosyntheseTekst", String.format("Tomosynthese (%s)", StringUtils.capitalize(zijde)));
		sliceTomosyntheseTekst.setVisible(toonAfwijkingSliceButtons);
		form.add(sliceTomosyntheseTekst);
	}

	private void renderAfkeurRedenen(ScreenitForm<MammaLezing> form)
	{
		Label afkeurreden;
		MammaBeoordeling beoordeling = getModelObject().getBeoordeling();
		if (beoordeling != null && MammaBeoordelingStatus.VERSLAG_AFGEKEURD.equals(beoordeling.getStatus()))
		{
			String afkeurredenTekst = StringUtils.isNotBlank(beoordeling.getAfkeurreden())
				? String.format("Verslag afgekeurd met reden: %s", beoordeling.getAfkeurreden())
				: "Geen afkeurreden opgegeven";
			afkeurreden = new Label("afkeurreden", afkeurredenTekst);
		}
		else
		{
			afkeurreden = new Label("afkeurreden", "");
			afkeurreden.setVisible(false);
		}
		form.add(afkeurreden);

	}

	private void onAfbeeldingGewijzigd(AjaxRequestTarget target)
	{
		target.add(laesiesContainer);
	}

	private WebMarkupContainer createLaesieLijst()
	{
		return new ListView<LaesieDto>("laesieLijst", laesieDtos)
		{
			@Override
			protected void populateItem(ListItem<LaesieDto> item)
			{
				item.add(createLaesiePanel(new CompoundPropertyModel<>(item.getModel())));
			}
		};
	}

	private void koppelLaesiesAanLezingEnSlaOp(IModel<MammaLezing> verslagLezingModel, MammaVerslagRondePanel verslagPanel)
	{
		verslagPanel.koppelNieuweLaesiesAanLezing(verslagLezingModel, laesieDtos.getObject());
		baseBeoordelingService.slaLezingOp(verslagPanel.getModelObject(), verslagLezingModel.getObject());
	}

	private void gaNaarVerwijsVerslagPanel(AjaxRequestTarget target, MammaVerslagRondePanel verslagPanel, IModel<MammaLezing> verslagLezingModel, MammaAmputatie amputatie,
		boolean toonAfwijkingSliceButtons)
	{
		MammaVerwijsVerslagPanel verwijsVerslagPanel = new MammaVerwijsVerslagPanel("verslagPanel", verslagPanel, verslagLezingModel, amputatie, toonAfwijkingSliceButtons);
		verwijsVerslagPanel.setOutputMarkupId(true);
		verslagPanel.replaceRonde(target, verwijsVerslagPanel);
	}

	private void valideerBirads(MammaLezing lezing)
	{
		if (lezing.getBiradsRechts() == null)
		{
			error(getString("birads.rechts"));
		}
		if (lezing.getBiradsLinks() == null)
		{
			error(getString("birads.links"));
		}

		if (!baseBeoordelingService.isLezingVerwijzen(lezing))
		{
			error(getString("birads.niet.verwijzen"));
		}

		if (!beoordelingService.isLezingValide(lezing, laesieDtos.getObject()))
		{
			error(getString("error.lezing.annotatie.en.birads"));
		}
	}

	private LaesiePanel createLaesiePanel(IModel<LaesieDto> model)
	{
		switch (model.getObject().getLaesietype())
		{
		case MASSA:
			return new MassaLaesiePanel("laesie", model, laesieDtos.getObject());
		case CALCIFICATIES:
			return new CalcificatiesLaesiePanel("laesie", model, laesieDtos.getObject());
		case ARCHITECTUURVERSTORING:
			return new ArchitectuurverstoringLaesiePanel("laesie", model, laesieDtos.getObject());
		case ASYMMETRIE:
			return new AsymmetrieLaesiePanel("laesie", model, laesieDtos.getObject());
		default:
			throw new IllegalStateException("Ongeldig laesieType: " + model.getObject().getLaesietype());
		}
	}

	public void blokeerOpslaan()
	{
		maakVerslagBtn.setEnabled(false);
		AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
		if (target != null)
		{
			target.add(maakVerslagBtn);
		}
	}
}
