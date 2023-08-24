package nl.rivm.screenit.main.web.gebruiker.testen.colon.timeline.popups;

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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.colon.ColonTestTimelineService;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.testen.gedeeld.timeline.popups.AbstractTestBasePopupPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonGeinterpreteerdeUitslag;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.IFOBTType;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.service.colon.ColonStudietestService;
import nl.rivm.screenit.service.impl.ProjectUitslagenUploadException;
import nl.rivm.screenit.util.FITTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.SimpleListHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.Radio;
import org.apache.wicket.markup.html.form.RadioGroup;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TestIfobtTestPopup extends AbstractTestBasePopupPanel
{
	@SpringBean
	private ColonTestTimelineService colonTestTimelineService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private ColonStudietestService studietestService;

	@SpringBean
	private HibernateService hibernateService;

	private final IModel<IFOBTTest> buisModel;

	private final IModel<List<IFOBTTest>> buizenListModel;

	private IModel<IFOBTTest> testModel;

	private WebMarkupContainer ifobtTestContainer;

	private final ScreenitDropdown<IFOBTTest> buisDropDown;

	private final IModel<Boolean> verlopenModel = Model.of(false);

	private HashMap<Long, SimpleListHibernateModel<IFOBTTest>> buizenMap = new HashMap<>();

	public TestIfobtTestPopup(String id, IModel<List<Client>> clientModel)
	{
		super(id, clientModel);
		buisModel = ModelUtil.ccModel(new IFOBTTest());

		ifobtTestContainer = getIfobtContainer();
		ifobtTestContainer.setVisible(false);
		add(ifobtTestContainer);

		List<IFOBTTest> buizenZonderUitslag = new ArrayList<>();
		for (ColonScreeningRonde ronde : getModelObject().get(0).getColonDossier().getScreeningRondes())
		{
			for (IFOBTTest buis : ronde.getIfobtTesten())
			{
				if (!FITTestUtil.heeftUitslag(buis))
				{
					buizenZonderUitslag.add(buis);
					List<IFOBTTest> buizen = new ArrayList<>();
					buizen.add(buis);
					SimpleListHibernateModel<IFOBTTest> buizenModel = new SimpleListHibernateModel<>(buizen);
					buizenMap.put(buis.getId(), buizenModel);
				}
			}
		}
		buizenListModel = ModelUtil.listModel(buizenZonderUitslag);

		if (getModelObject().size() > 1)
		{
			for (Client client : getModelObject().subList(1, getModelObject().size()))
			{
				for (ColonScreeningRonde ronde : client.getColonDossier().getScreeningRondes())
				{
					for (int i = 0; i < ronde.getIfobtTesten().size(); i++)
					{
						IFOBTTest test = ronde.getIfobtTesten().get(i);
						if (!FITTestUtil.heeftUitslag(test))
						{
							SimpleListHibernateModel<IFOBTTest> testBuizen = buizenMap.get(buizenZonderUitslag.get(i).getId());
							testBuizen.add(test);
						}
					}
				}
			}
		}
		buisDropDown = new ScreenitDropdown<>("uitnodigingen", buisModel, buizenListModel, new IChoiceRenderer<>()
		{
			@Override
			public Object getDisplayValue(IFOBTTest test)
			{
				return "Buis(" + test.getBarcode() + ")/Uitnodiging(" + FITTestUtil.getUitnodiging(test).getUitnodigingsId() + ")";
			}

			@Override
			public String getIdValue(IFOBTTest test, int index)
			{
				if (test.getId() != null)
				{
					return test.getId().toString();
				}
				return null;
			}

			@Override
			public IFOBTTest getObject(String id, IModel<? extends List<? extends IFOBTTest>> choices)
			{
				if (id != null)
				{
					return choices.getObject().stream().filter(i -> i.getId().toString().equals(id)).findFirst().orElse(null);
				}
				return null;
			}

		});
		buisDropDown.setNullValid(true);
		buisDropDown.setRequired(true);
		buisDropDown.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				IFOBTTest buis = buisDropDown.getConvertedInput();
				if (buis != null)
				{
					testModel = ModelUtil.ccModel(buis);
				}
				else
				{
					throw new IllegalStateException("Er moet een ifobttest aanwezig zijn!");
				}
				WebMarkupContainer container = getIfobtContainer();
				ifobtTestContainer.replaceWith(container);
				ifobtTestContainer = container;
				target.add(ifobtTestContainer);

			}

			@Override
			protected boolean getUpdateModel()
			{
				return false;
			}
		});
		add(buisDropDown);

	}

	private WebMarkupContainer getIfobtContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("ifobtContainer");
		container.setOutputMarkupPlaceholderTag(true);

		boolean studieTest = testModel != null && testModel.getObject().getType() == IFOBTType.STUDIE;
		TextField<BigDecimal> uitslagText = new TextField<>("uitslag", new PropertyModel<>(testModel, "uitslag"));
		container.add(uitslagText.setOutputMarkupId(true).setVisible(!studieTest));

		DropDownChoice<ColonGeinterpreteerdeUitslag> geinterpreteerdeUitslagDropDown = new ScreenitDropdown<>("geinterpreteerdeUitslag",
			new PropertyModel<>(testModel, "geinterpreteerdeUitslag"), Arrays.asList(ColonGeinterpreteerdeUitslag.values()), new EnumChoiceRenderer<>());
		container.add(geinterpreteerdeUitslagDropDown.setRequired(true).setVisible(studieTest));

		Model<BigDecimal> uitslagValueModel = Model.of(new BigDecimal(-1));
		RadioGroup<BigDecimal> radioGroup = new RadioGroup<>("uitslagSnel", uitslagValueModel);
		radioGroup.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				testModel.getObject().setUitslag(uitslagValueModel.getObject());
				target.add(uitslagText);
			}

		});

		BigDecimal normWaardeGold = BigDecimal.valueOf(preferenceService.getInteger(PreferenceKey.IFOBT_NORM_WAARDE.name())).divide(BigDecimal.valueOf(100));
		radioGroup.setRequired(true);
		add(radioGroup);
		radioGroup.add(new Radio<>("gunstig", Model.of(BigDecimal.ZERO)));
		radioGroup.add(new Radio<>("ongunstig", Model.of(normWaardeGold.add(BigDecimal.ONE))));
		container.add(radioGroup.setVisible(!studieTest));

		container.add(new CheckBox("verlopen", verlopenModel));

		return container;
	}

	@Override
	protected void opslaan()
	{
		List<IFOBTTest> testBuizen = new ArrayList<>();
		if (testModel.getObject().getId() != null)
		{
			Long eersteBuisId = testModel.getObject().getId();
			if (buizenMap.containsKey(eersteBuisId))
			{
				testBuizen = buizenMap.get(eersteBuisId).getObject();
			}
			for (IFOBTTest buis : testBuizen)
			{
				if (!IFOBTTestStatus.NIETTEBEOORDELEN.equals(buis.getStatus()))
				{
					buis.setUitslag(testModel.getObject().getUitslag());
					if (buis.getType().equals(IFOBTType.STUDIE))
					{
						try
						{
							studietestService.controleerUitslagenbestandOpFouten(buis, null);
							colonTestTimelineService.ifobtTestOntvangen(buis.getColonScreeningRonde().getDossier().getClient(), verlopenModel.getObject(), buis, 1);
						}
						catch (ProjectUitslagenUploadException e)
						{
							buis.setGeinterpreteerdeUitslag(null);
							hibernateService.saveOrUpdate(buis);
							error(e.getMessage());
						}
					}
					else
					{
						checkEnVerwijderGeinterpreteerdeUitslagExtraBuis(buis);
						colonTestTimelineService.ifobtTestOntvangen(buis.getColonScreeningRonde().getDossier().getClient(), verlopenModel.getObject(), buis, 1);
					}
				}
			}
		}
	}

	private void checkEnVerwijderGeinterpreteerdeUitslagExtraBuis(IFOBTTest buis)
	{
		var uitnodiging = buis.getColonUitnodiging();
		if (uitnodiging != null && uitnodiging.getGekoppeldeExtraTest() != null)
		{
			var andereFIT = uitnodiging.getGekoppeldeExtraTest();
			if (IFOBTType.STUDIE == andereFIT.getType() && andereFIT.getVerwerkingsDatum() == null && andereFIT.getGeinterpreteerdeUitslag() != null)
			{
				andereFIT.setGeinterpreteerdeUitslag(null);
			}
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		for (IModel<List<IFOBTTest>> buizen : buizenMap.values())
		{
			ModelUtil.nullSafeDetach(buizen);
		}
		ModelUtil.nullSafeDetach(buisModel);
		ModelUtil.nullSafeDetach(testModel);
	}
}
