package nl.rivm.screenit.main.web.gebruiker.testen.hpvbericht;

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

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.cervix.HpvSendingMessageService;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.berichten.ScreenITResponseV251MessageWrapper;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvOrderCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.enums.CervixHpvResultaatBerichtBron;
import nl.rivm.screenit.repository.cervix.CervixHpvBerichtRepository;
import nl.rivm.screenit.specification.cervix.CervixHpvBerichtSpecification;
import nl.rivm.screenit.util.cervix.hpv_berichtgenerator.CervixHpvBerichtGenerator;
import nl.rivm.screenit.util.cervix.hpv_berichtgenerator.CervixHpvBerichtGeneratorMonsterWrapper;
import nl.rivm.screenit.util.cervix.hpv_berichtgenerator.CervixHpvBerichtGeneratorWrapper;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.PropertyListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import ca.uhn.hl7v2.model.Message;
import ca.uhn.hl7v2.util.StringUtil;

public class TestHpvBerichtPanel extends GenericPanel<CervixHpvBerichtGeneratorWrapper>
{
	private static final long serialVersionUID = 1L;

	private WebMarkupContainer uitslagenContainer;

	@SpringBean
	private CervixHpvBerichtRepository hpvBerichtRepository;

	@SpringBean
	private HpvSendingMessageService hpvSendingMessageService;

	public TestHpvBerichtPanel(String id)
	{
		super(id, new CompoundPropertyModel<>(new CervixHpvBerichtGeneratorWrapper()));
		CervixHpvBerichtGeneratorWrapper wrapper = getModelObject();
		wrapper.getMonsterWrappers().add(new CervixHpvBerichtGeneratorMonsterWrapper());

		Form<CervixHpvBerichtGeneratorWrapper> form = new Form<>("form", getModel());
		add(form);

		ComponentHelper.addTextField(form, "messageId", true, 100, false);
		ComponentHelper.addTextField(form, "instrumentId", true, 100, false);
		ComponentHelper.addTextField(form, "labNaam", true, 100, false);
		ComponentHelper.addDropDownChoice(form, "resultaatBerichtBron", true, Arrays.asList(CervixHpvResultaatBerichtBron.values()), false);

		uitslagenContainer = getUitslagenContainer();
		form.add(uitslagenContainer);

		form.add(new IndicatingAjaxSubmitLink("verstuurHl7Bericht", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				CervixHpvBerichtGeneratorWrapper wrapper = getModelObject();
				String alAangeleverd = "";
				if (hpvBerichtRepository.exists(CervixHpvBerichtSpecification.heeftMessageId(wrapper.getMessageId())))
				{
					alAangeleverd = ", Bericht al een keer aangeleverd met dit messageId. Dit bericht wordt daarom niet meer verwerkt door de batch maar levert wel een AA op!";
				}

				Message hl7bericht = CervixHpvBerichtGenerator.geefHL7Bericht(wrapper);

				ScreenITResponseV251MessageWrapper result = null;
				try
				{
					result = hpvSendingMessageService.verstuurHpvBericht(hl7bericht);
				}
				catch (IOException e)
				{
					throw new RuntimeException(e);
				}

				String melding = "Bericht verstuurd, Code: ";
				if (result != null)
				{
					melding += result.getAcknowledgmentCode();
					if (result.getMelding() != null)
					{
						melding += ", reden: " + result.getMelding();
					}
				}
				else
				{
					melding += "<geen response>";
				}
				if (StringUtil.isNotBlank(alAangeleverd))
				{
					melding += alAangeleverd;
				}
				info(melding);

			}
		});

		form.add(new IndicatingAjaxSubmitLink("uitslagToevoegen", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper = new CervixHpvBerichtGeneratorMonsterWrapper();
				getModelObject().getMonsterWrappers().add(monsterWrapper);
				target.add(uitslagenContainer);
			}
		});
	}

	private WebMarkupContainer getUitslagenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("uitslagenContainer");
		container.setOutputMarkupId(true);

		PropertyListView<CervixHpvBerichtGeneratorMonsterWrapper> list = new PropertyListView<>("monsterWrappers",
			new PropertyModel<>(getModel(), "monsterWrappers"))
		{
			@Override
			protected void populateItem(final ListItem<CervixHpvBerichtGeneratorMonsterWrapper> item)
			{
				item.add(ComponentHelper.addTextField(this, "barcode", true, 100, false));
				var orderCodes = new ListModel<>(Arrays.asList(CervixHpvOrderCode.values()));
				var analyseCodes = new ListModel<>(Arrays.asList(CervixHpvResultCode.values()));
				var resultaatWaardes = new ListModel<>(Arrays.asList(CervixHpvResultValue.values()));
				item.add(new DropDownChoice<>("ordercode", orderCodes, new EnumChoiceRenderer<>()).setRequired(true));
				item.add(new DropDownChoice<>("analysecode1", analyseCodes, new EnumChoiceRenderer<>()));
				item.add(new DropDownChoice<>("analyseresultaat1", resultaatWaardes, new EnumChoiceRenderer<>()));
				item.add(new DropDownChoice<>("analysecode2", analyseCodes, new EnumChoiceRenderer<>()));
				item.add(new DropDownChoice<>("analyseresultaat2", resultaatWaardes, new EnumChoiceRenderer<>()));
				item.add(new DropDownChoice<>("analysecode3", analyseCodes, new EnumChoiceRenderer<>()));
				item.add(new DropDownChoice<>("analyseresultaat3", resultaatWaardes, new EnumChoiceRenderer<>()));

				item.add(ComponentHelper.monthYearDatePicker("autorisatieDatum").setRequired(true));
				item.add(ComponentHelper.monthYearDatePicker("analyseDatum").setRequired(true));

				item.add(new IndicatingAjaxLink<CervixHpvBerichtGeneratorWrapper>("verwijderen")
				{
					@Override
					public void onClick(AjaxRequestTarget target)
					{
						CervixHpvBerichtGeneratorMonsterWrapper monsterWrapper = item.getModelObject();

						List<CervixHpvBerichtGeneratorMonsterWrapper> wrappers = TestHpvBerichtPanel.this.getModelObject().getMonsterWrappers();
						wrappers.remove(monsterWrapper);
						target.add(uitslagenContainer);
					}
				});
			}

		};
		container.add(list);
		return container;
	}
}
