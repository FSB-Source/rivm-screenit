package nl.rivm.screenit.batch.jobs.brieven.genereren;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-base
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

import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.ParameterizedType;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.BatchConstants;
import nl.rivm.screenit.document.BaseDocumentCreator;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.IDocument;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.impl.IBrievenGeneratorHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.services.impl.OpenHibernate5Session;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.annotation.BeforeStep;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.NamedThreadLocal;

import com.aspose.words.Document;

public abstract class AbstractBrievenGenererenWriter<T extends Brief, S extends MergedBrieven<?>> implements ItemStreamWriter<T>, IBrievenGeneratorHelper<T, S>
{
	private static final Logger LOG = LoggerFactory.getLogger(AbstractBrievenGenererenWriter.class);

	private static final ThreadLocal<Map<Object, Object>> resources = new NamedThreadLocal<Map<Object, Object>>("Brief writer resources");

	private static final String KEY_MERGEDDOCUMENTID = "mergedDocumentid";

	private static final String KEY_BRIEFTYPE = "briefType";

	private static final String KEY_BRIEVEN = "zijnErBrieven";

	private static final String KEY_PDF_COUNTER = "pdfCounter";

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private BaseBriefService briefService;

	@Autowired
	private ICurrentDateSupplier dateSupplier;

	private JobExecution jobExecution;

	private StepExecution stepExecution;

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		OpenHibernate5Session.withCommittedTransaction().run(() -> {

			Map<Object, Object> resourcesMap = resources.get();
			if (resourcesMap == null)
			{
				resources.set(new HashMap<>());
				resourcesMap = resources.get();
			}

			S mergedBrieven = null;
			BriefType briefType = null;
			if (executionContext.containsKey(KEY_MERGEDDOCUMENTID))
			{
				try
				{
					mergedBrieven = hibernateService.load(getMergedBrievenClass(), executionContext.getLong(KEY_MERGEDDOCUMENTID));
				}
				catch (Exception e)
				{
					LOG.error("Error loading merged brieven file", e);
				}
			}
			else
			{
				mergedBrieven = createMergedBrieven(dateSupplier.getDate());
			}

			briefType = mergedBrieven.getBriefType();

			resourcesMap.put(KEY_BRIEFTYPE, briefType);
		});

	}

	@SuppressWarnings("unchecked")
	protected final Class<S> getMergedBrievenClass()
	{
		return (Class<S>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[1];
	}

	@Override
	public void write(List<? extends T> items) throws Exception
	{
		briefService.createOrAddMergedBrieven(items, this);
	}

	protected abstract S createConcreteMergedBrieven(Date aangemaaktOp);

	@Override
	public S createMergedBrieven(Date aangemaaktOp)
	{
		S mergedBrieven = createConcreteMergedBrieven(aangemaaktOp);
		hibernateService.saveOrUpdate(mergedBrieven);
		getStepExecutionContext().put(KEY_MERGEDDOCUMENTID, mergedBrieven.getId());
		resources.get().put(KEY_MERGEDDOCUMENTID, mergedBrieven.getId());

		return mergedBrieven;
	}

	@Override
	public S getMergedBrieven()
	{
		return hibernateService.load(getMergedBrievenClass(), (Long) resources.get().get(KEY_MERGEDDOCUMENTID));
	}

	@Override
	public IDocument getDocumentDefinitie()
	{
		BriefType briefType = (BriefType) resources.get().get(KEY_BRIEFTYPE);
		return briefService.getNieuwsteBriefDefinitie(briefType);
	}

	@Override
	public void additionalActiesWithDocument(MailMergeContext context, T brief, Document chunkDocument) throws Exception
	{
	}

	@Override
	public Long getFileStoreId()
	{
		return null;
	}

	@Override
	public String getTechnischeLoggingMergedBriefAanmaken(S brieven)
	{
		;
		String tekst = "Mergedocument(id = " + brieven.getId() + ") aangemaakt voor ScreeningOrganisatie " + brieven.getScreeningOrganisatie().getNaam() + ", brieftype "
			+ brieven.getBriefType().name() + ", #" + getStepExecutionContext().getInt(KEY_PDF_COUNTER, 1);
		return tekst;
	}

	@Override
	public void crashMelding(String melding, Exception e)
	{
		LOG.error(melding, e);
		getExecutionContext().put(BatchConstants.MELDING, melding);
		getExecutionContext().put(BatchConstants.LEVEL, Level.ERROR);
	}

	protected ExecutionContext getExecutionContext()
	{
		return jobExecution.getExecutionContext();
	}

	protected ExecutionContext getStepExecutionContext()
	{
		return stepExecution.getExecutionContext();
	}

	@BeforeStep
	public void saveStepExecution(StepExecution stepExecution)
	{
		this.stepExecution = stepExecution;
		this.jobExecution = stepExecution.getJobExecution();
	}

	@Override
	public String getMergedBrievenNaam(S brieven)
	{
		String naam = "";
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd_HH.mm");
		if (brieven.getCreatieDatum() != null)
		{
			naam += sdf.format(brieven.getCreatieDatum()) + "-";
		}
		if (brieven.getScreeningOrganisatie() != null)
		{
			String soNaam = brieven.getScreeningOrganisatie().getNaam();
			soNaam = soNaam.replaceAll(" ", "_");
			naam += soNaam + "-";
		}
		if (brieven.getBriefType() != null)
		{
			naam += brieven.getBriefType().name().toLowerCase();
		}
		naam = addPdfCounter(naam);

		return naam + ".pdf";
	}

	protected String addPdfCounter(String naam)
	{
		if (getStepExecutionContext().containsKey(KEY_PDF_COUNTER))
		{
			naam += "_" + StringUtils.leftPad(getStepExecutionContext().getInt(KEY_PDF_COUNTER) + "", 2, '0');
		}
		return naam;
	}

	@Override
	public void additionalMergedContext(MailMergeContext context)
	{
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{

	}

	@Override
	public void close() throws ItemStreamException
	{
		OpenHibernate5Session.withCommittedTransaction().run(() -> {
			FileOutputStream outputStream = null;
			try
			{
				Map<Object, Object> resourcesMap = resources.get();
				Long id = (Long) resourcesMap.get(KEY_MERGEDDOCUMENTID);
				S mergedBrieven = hibernateService.load(getMergedBrievenClass(), id);
				boolean heeftBrieven = stepExecution.getExecutionContext().containsKey(KEY_BRIEVEN);

				if (!heeftBrieven)
				{
					hibernateService.delete(mergedBrieven);
				}
				else
				{
					outputStream = briefService.completeEnGetPdf(mergedBrieven);
				}
			}
			catch (IOException e)
			{
				crashMelding("Javascript kon niet aan de mergedbrieven worden toegevoegd", e);
				throw new ItemStreamException("Javascript kon niet aan de mergedbrieven worden toegevoegd");
			}
			finally
			{
				if (outputStream != null)
				{
					try
					{
						outputStream.close();
					}
					catch (IOException e)
					{
						crashMelding("Outputstream kon niet worden afgesloten!", e);
						throw new ItemStreamException("Outputstream kon niet worden afgesloten!");
					}
				}
			}
			resources.remove();
		});

	}

	protected abstract String getRapportageAantalBrievenKey();

	@Override
	public Bevolkingsonderzoek[] getBevolkingsonderzoeken()
	{
		return null;
	}

	@Override
	public BaseDocumentCreator getDocumentCreator(MailMergeContext context)
	{
		return null;
	}

	@Override
	public void verhoogAantalBrievenVanScreeningOrganisatie(S mergedBrieven)
	{
		Map<Long, Integer> map = (Map<Long, Integer>) stepExecution.getJobExecution().getExecutionContext().get(getRapportageAantalBrievenKey());
		if (map == null)
		{
			map = new HashMap<>();
			getExecutionContext().put(getRapportageAantalBrievenKey(), map);
		}
		Integer currentValue = map.get(mergedBrieven.getScreeningOrganisatie().getId());
		if (currentValue == null)
		{
			currentValue = 0;
		}
		map.put(mergedBrieven.getScreeningOrganisatie().getId(), currentValue + mergedBrieven.getAantalBrieven());
		stepExecution.getExecutionContext().putString(KEY_BRIEVEN, "JA");
	}

	protected final HibernateService getHibernateService()
	{
		return hibernateService;
	}

	@Override
	public void increasePdfCounter()
	{
		getStepExecutionContext().putInt(KEY_PDF_COUNTER, getStepExecutionContext().getInt(KEY_PDF_COUNTER, 1) + 1);
	}
}
