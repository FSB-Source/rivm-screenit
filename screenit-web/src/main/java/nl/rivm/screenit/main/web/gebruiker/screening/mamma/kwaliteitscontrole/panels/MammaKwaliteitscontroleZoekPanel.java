package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.panels;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.model.mamma.beoordeling.MammaKwaliteitscontroleWerklijstZoekObject;
import nl.rivm.screenit.main.service.mamma.MammaBeoordelingsEenheidService;
import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitListMultipleChoice;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaKwaliteitscontroleZoekPanel<T extends MammaKwaliteitscontroleWerklijstZoekObject> extends GenericPanel<T>
{
	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private MammaBeoordelingsEenheidService beoordelingsEenheidService;

	private ScreenitListMultipleChoice<BeoordelingsEenheid> beoordelingseenhedenSelector;

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> screeningsEenhedenSelector;

	private final Form<T> zoekForm;

	public MammaKwaliteitscontroleZoekPanel(String id, IModel<T> model)
	{
		super(id, model);
		zoekForm = new Form<>("form", getModel());
		add(zoekForm);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		fillZoekForm();
		zetZoekobjectModel();
	}

	private void fillZoekForm()
	{
		List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden = getMogelijkeBeoordelingsEenheden();

		WebMarkupContainer centraleEenhedenContainer = new WebMarkupContainer("centraleEenhedenContainer");
		centraleEenhedenContainer.add(createCentraleEenhedenSelector());
		centraleEenhedenContainer.setOutputMarkupId(true);

		WebMarkupContainer beoordelingsEenhedenContainer = new WebMarkupContainer("beoordelingsEenhedenContainer");
		beoordelingseenhedenSelector = createBeoordelingseenhedenSelector(mogelijkeBeoordelingsEenheden);
		beoordelingsEenhedenContainer.add(beoordelingseenhedenSelector);
		beoordelingsEenhedenContainer.setOutputMarkupId(true);

		WebMarkupContainer screeningsEenhedenContainer = new WebMarkupContainer("screeningsEenhedenContainer");
		screeningsEenhedenSelector = createScreeningsEenhedenSelector(mogelijkeBeoordelingsEenheden);
		screeningsEenhedenContainer.add(screeningsEenhedenSelector);
		screeningsEenhedenContainer.setOutputMarkupId(true);

		zoekForm.add(centraleEenhedenContainer);
		zoekForm.add(beoordelingsEenhedenContainer);
		zoekForm.add(screeningsEenhedenContainer);
		addAdditionalZoekFields(zoekForm);
		addZoekButton();
	}

	protected void addAdditionalZoekFields(Form<T> zoekForm)
	{
	}

	private ScreenitListMultipleChoice<MammaScreeningsEenheid> createScreeningsEenhedenSelector(List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden)
	{
		List<MammaScreeningsEenheid> mogelijkeScreeningsEenheden = getMogelijkeScreeningsEenheden(mogelijkeBeoordelingsEenheden);
		return new ScreenitListMultipleChoice<>("screeningsEenheden", ModelUtil.listRModel(mogelijkeScreeningsEenheden), new ChoiceRenderer<>("naam"));
	}

	private void addZoekButton()
	{
		IndicatingAjaxSubmitLink zoekenButton = new IndicatingAjaxSubmitLink("zoeken", zoekForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				zetZoekobjectModel();
				MammaKwaliteitscontroleZoekPanel.this.onZoeken(target, MammaKwaliteitscontroleZoekPanel.this.getModel());
				target.add(zoekForm);
			}
		};
		zoekForm.setDefaultButton(zoekenButton);
		zoekForm.add(zoekenButton);
	}

	protected abstract void onZoeken(AjaxRequestTarget target, IModel<T> zoekModel);

	private void zetZoekobjectModel()
	{
		MammaKwaliteitscontroleWerklijstZoekObject zoekObject = getModelObject();
		if (CollectionUtils.isEmpty(zoekObject.getBeoordelingsEenheden()))
		{
			zoekObject.setBeoordelingsEenheden(getMogelijkeBeoordelingsEenheden(zoekObject.getCentraleEenheden()));
		}
		if (CollectionUtils.isEmpty(zoekObject.getScreeningsEenheden()))
		{
			resetSeKeuzelijst();
		}
		ScreenitSession.get().setZoekObject(getPage().getPageClass(), zoekForm.getModel());
	}

	private ScreenitListMultipleChoice<CentraleEenheid> createCentraleEenhedenSelector()
	{
		List<CentraleEenheid> mogelijkeCentraleEenheden = getMogelijkeCentraleEenheden();
		ScreenitListMultipleChoice<CentraleEenheid> centraleEenhedenSelector = new ScreenitListMultipleChoice<>("centraleEenheden",
			ModelUtil.listRModel(mogelijkeCentraleEenheden), new ChoiceRenderer<>("naam"));

		centraleEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				resetBeKeuzelijst();
				target.add(zoekForm);
			}
		});

		return centraleEenhedenSelector;
	}

	private void resetBeKeuzelijst()
	{
		MammaKwaliteitscontroleWerklijstZoekObject zoekObject = getModelObject();
		List<CentraleEenheid> gekozenCEs = zoekObject.getCentraleEenheden();
		List<BeoordelingsEenheid> mogelijkeBEs = getMogelijkeBeoordelingsEenheden(gekozenCEs);
		beoordelingseenhedenSelector.setChoices(ModelUtil.listRModel(mogelijkeBEs));
		zoekObject.setBeoordelingsEenheden(mogelijkeBEs);
		resetSeKeuzelijst();
	}

	private ScreenitListMultipleChoice<BeoordelingsEenheid> createBeoordelingseenhedenSelector(List<BeoordelingsEenheid> mogelijkeBeoordelingsEenheden)
	{
		ScreenitListMultipleChoice<BeoordelingsEenheid> beoordelingsEenhedenSelector = new ScreenitListMultipleChoice<>("beoordelingsEenheden",
			ModelUtil.listRModel(mogelijkeBeoordelingsEenheden), new ChoiceRenderer<>("naam"));

		beoordelingsEenhedenSelector.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				resetSeKeuzelijst();
				target.add(zoekForm);
			}
		});

		return beoordelingsEenhedenSelector;
	}

	private void resetSeKeuzelijst()
	{
		MammaKwaliteitscontroleWerklijstZoekObject zoekObject = getModelObject();
		List<BeoordelingsEenheid> gekozenBEs = zoekObject.getBeoordelingsEenheden();
		List<MammaScreeningsEenheid> mogelijkeSEs = getMogelijkeScreeningsEenheden(gekozenBEs);
		screeningsEenhedenSelector.setChoices(ModelUtil.listRModel(mogelijkeSEs));
		zoekObject.setScreeningsEenheden(mogelijkeSEs);
	}

	private List<MammaScreeningsEenheid> getMogelijkeScreeningsEenheden(List<BeoordelingsEenheid> beoordelingsEenheden)
	{
		MammaKwaliteitscontroleWerklijstZoekObject zoekObject = getModelObject();
		if (CollectionUtils.isEmpty(beoordelingsEenheden))
		{
			beoordelingsEenheden = getMogelijkeBeoordelingsEenheden(zoekObject.getCentraleEenheden());
		}
		return screeningsEenheidService.getActieveScreeningsEenhedenVoorBeoordelingsEenheden(beoordelingsEenheden);
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden()
	{
		return beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getInstelling());
	}

	private List<BeoordelingsEenheid> getMogelijkeBeoordelingsEenheden(List<CentraleEenheid> centraleEenheden)
	{
		return CollectionUtils.isEmpty(centraleEenheden) ? beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getInstelling())
			: beoordelingsEenheidService.getBeoordelingsEenheden(ScreenitSession.get().getInstelling(), centraleEenheden);
	}

	private List<CentraleEenheid> getMogelijkeCentraleEenheden()
	{
		return instellingService.getMogelijkeCentraleEenheden(ScreenitSession.get().getInstelling());
	}

}
